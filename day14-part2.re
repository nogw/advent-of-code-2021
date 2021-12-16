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

let change_to_freq = (t, p) => {
  let freq =
    List.fold_left(
      (acc, k) =>
        switch (Pairs.find_opt(k, acc)) {
        | Some(v) => Pairs.add(k, succ(v), acc)
        | None => Pairs.add(k, 1, acc)
        },
      Pairs.map(_ => 0, p),
      two(t),
    );
  (freq, p);
};

module Freq = Map.Make(Char);

let f = (freq, p) => {
  let step = (freq, p) =>
    Pairs.fold(
      (pair, count, acc) =>
        switch (Pairs.find_opt(pair, p)) {
        | Some(c) =>
          let np = String.sub(pair, 0, 1) ++ c;
          let np' = c ++ String.sub(pair, 1, 1);

          acc
          |> Pairs.add(np, Pairs.find(np, acc) + count)
          |> Pairs.add(np', Pairs.find(np', acc) + count);
        | None => acc
        },
      freq,
      Pairs.map(_ => 0, p),
    );

  let rec run = (freq, p) => {
    fun
    | 0 => freq
    | v => run(step(freq, p), p, pred(v));
  };

  run(freq, p, 40)
  |> (
    x =>
      Pairs.fold(
        (pair, count, acc) => {
          let c1 = String.sub(pair, 0, 1);
          let c2 = String.sub(pair, 1, 1);

          acc
          |> Pairs.add(
               c1,
               switch (Pairs.find_opt(c1, acc)) {
               | Some(i) => i + count
               | None => count
               },
             )
          |> (
            x =>
              Pairs.add(
                c2,
                switch (Pairs.find_opt(c2, x)) {
                | Some(i) => i + count
                | None => count
                },
                x,
              )
          );
        },
        x,
        Pairs.empty,
      )
      |> Pairs.map(x => x mod 2 == 0 ? x / 2 : x / 2 + 1)
  )
  |> (
    x =>
      Pairs.fold(
        (_, i, (max, min)) =>
          i < min && i > max
            ? (i, i) : i < min ? (max, i) : i > max ? (i, min) : (max, min),
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
  change_to_freq
  |> uncurry @@
  f
  |> print_int
  |> print_newline;
