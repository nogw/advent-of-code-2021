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

let findi = {
  let rec aux = (i, pred, l) => {
    switch (l) {
    | [] => raise(Not_found)
    | [h, ..._] when pred(h) => i
    | [_, ...t] => aux(succ(i), pred, t)
    };
  };

  aux(0);
};

let rec take = (n, l) => {
  n <= 0
    ? []
    : (
      switch (l) {
      | [] => []
      | [h, ...t] => [h, ...take(pred(n), t)]
      }
    );
};

let rec drop = (n, l) => {
  n === 0
    ? l
    : {
      switch (l) {
      | [] => []
      | [_, ...t] => drop(pred(n), t)
      };
    };
};

module Pairs = Map.Make(String);

let to_char_list = s => List.init(String.length(s), String.get(s));

let parse = l => {
  let i = findi((==)(""), l);
  let tmpt = take(i, l) |> List.hd |> to_char_list;
  let pairs =
    drop(succ(i), l)
    |> List.map(s => Scanf.sscanf(s, "%s -> %s", (x, y) => (x, y)))
    |> List.fold_left((acc, (x, y)) => Pairs.add(x, y, acc), Pairs.empty);
  (tmpt, pairs);
};

let uncurry = (f, (x, y)) => f(x, y);

let two = x => {
  List.combine(x, List.tl(x) @ [' '])
  |> take(List.length(x) - 1)
  |> List.map(((x, y)) => String.make(1, x) ++ String.make(1, y));
};

module Freq = Map.Make(Char);

let f = (t, p) => {
  let step = (t, p) => {
    t
    |> two
    |> List.map(x =>
         String.sub(x, 0, 1) ++ Pairs.find(x, p) ++ String.sub(x, 1, 1)
       )
    |> List.mapi((i, x) =>
         i < List.length(t) - 2 ? String.sub(x, 0, 2) : x
       )
    |> List.fold_left((++), "")
    |> to_char_list;
  };

  let rec run = (t, p) => {
    fun
    | 0 => t
    | v => run(step(t, p), p, pred(v));
  };

  run(t, p, 10)
  |> List.fold_left(
       (m, c) =>
         switch (Freq.find_opt(c, m)) {
         | Some(i) => Freq.add(c, i + 1, m)
         | None => Freq.add(c, 1, m)
         },
       Freq.empty,
     )
  |> (
    x =>
      Freq.fold(
        (_, i, (max, min)) => {
          i < min && i > max
            ? (i, i) : i < min ? (max, i) : i > max ? (i, min) : (max, min)
        },
        x,
        (0, max_int),
      )
  )
  |> uncurry @@
  (-);
};

let () =
  read_lines("input.txt")
  |> parse
  |> uncurry @@
  f
  |> print_int
  |> print_newline;
