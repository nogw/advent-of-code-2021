let read_lines = name => {
  let ic = open_in(name);
  let try_read = () =>
    try(Some(input_line(ic))) {
    | End_of_file => None
    };
  let rec loop = acc =>
    switch (try_read()) {
    | Some(s) => loop([s, ...acc])
    | None =>
      close_in(ic);
      List.rev(acc);
    };
  loop([]);
};

module State = {
  type t = (((int, int), (int, int)), int);

  let compose = ((x, y), (x', y')) => {
    x == x' ? y - y' : x - x';
  };

  let compare = (((x1, y1), _), ((x2, y2), _)) => {
    compose(x1, x2) == 0 ? compose(y1, y2) : compose(x1, x2);
  };
};

module State = Set.Make(State);

let parse = l =>
  List.map(
    s =>
      String.split_on_char(':', s)
      |> (l => List.nth(l, List.length(l) - 1))
      |> (l => String.sub(l, 1, String.length(l) - 1))
      |> int_of_string
      |> (i => (i, 0)),
    l,
  )
  |> (l => State.of_list([((List.hd(l), List.hd @@ List.tl(l)), 1)]));

let play = g => {
  let aux = ((((x, xs), other), count), roll) => {
    let pos = (x + roll - 1) mod 10 + 1;

    ((other, (pos, xs + pos)), count);
  };

  State.fold(
    (game, acc_g) =>
      List.concat_map(
        e1 => List.map(e2 => (e1, e2), [1, 2, 3]),
        [1, 2, 3],
      )
      |> (
        y =>
          List.concat_map(
            ((e1, e2)) => (List.map(e3 => (e1, e2, e3)))([1, 2, 3]),
            y,
          )
      )
      |> List.map(((e1, e2, e3)) => e1 + e2 + e3)
      |> List.fold_left(
           (acc_g, d) => {
             let new_game = aux(game, d);
             let (_, count) = new_game;

             switch (State.find_opt(new_game, acc_g)) {
             | Some((t, c)) =>
               State.add((t, c + count), State.remove(new_game, acc_g))
             | None => State.add(new_game, acc_g)
             };
           },
           acc_g,
         ),
    g,
    State.empty,
  );
};

let run = {
  let rec aux = (x', y, x) =>
    if (State.is_empty(x)) {
      if (x' > y) {
        x';
      } else {
        y;
      };
    } else {
      let (wins, g) =
        State.fold(
          (game, (acc, new_g)) => {
            let (((_, _), (_, otherscore)), count) = game;

            if (otherscore >= 21) {
              (acc + count, State.remove(game, new_g));
            } else {
              (acc, new_g);
            };
          },
          x,
          (0, x),
        );

      aux(y, x' + wins, play(g));
    };

  aux(0, 0);
};

let () = read_lines("input.txt") |> parse |> run |> print_int;
