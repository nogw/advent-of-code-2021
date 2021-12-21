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

let parse =
  List.map(s =>
    String.split_on_char(':', s)
    |> (l => List.nth(l, List.length(l) - 1))
    |> (l => String.sub(l, 1, String.length(l) - 1))
    |> int_of_string
    |> (int => (int, 0))
  );

let play = ((lst, d)) => {
  [@warning "-8"]
  let [(x, xs), ...rest] = lst;
  let roll = d + (d mod 100 + 1) + (d + 1 mod 100 + 1);
  let pos = (x + roll - 1) mod 10 + 1;

  (rest @ [(pos, xs + pos)], (d + 2) mod 100 + 1);
};

let () =
  read_lines("input.txt")
  |> parse
  |> (
    l => {
      let rec aux = (acc, x) => {
        let (x', _) = x;
        let (_, xs) = List.hd(x');
        let (_, xs') = List.hd @@ List.tl(x');

        xs' >= 1000 ? 3 * acc * xs : aux(acc + 1, play(x));
      };

      aux(0, (l, 1));
    }
  )
  |> print_int;
