let parse = line => {
  let sub = (start, c) => {
    let i = String.index_from(line, start, c);
    let str = String.sub(line, start, i - start);
    (int_of_string(str), i + 1);
  };

  let (x, i) = sub(0, ',');
  let (y, i) = sub(i, ' ');
  let (z, i) = sub(i + 3, ',');
  let d = String.sub(line, i, String.length(line) - i) |> int_of_string;

  (x, y, z, d);
};

module LineHash = {
  type t = (int, int);
  let equal = ((x, y), (x', y')) => x == x' && y == y';
  let hash = (x: (int, int)) => Hashtbl.hash(x);
};

module LineHashtbl = Hashtbl.Make(LineHash);

let () = {
  let ic = open_in("input.txt");
  let table = LineHashtbl.create(1000);
  let least = ref(0);

  let diagonals = ref([]);

  let incr = point =>
    switch (LineHashtbl.find_opt(table, point)) {
    | Some(v) =>
      let y = v + 1;
      if (y == 2) {
        least := least^ + 1;
      };
      LineHashtbl.replace(table, point, v + 1);

    | None => LineHashtbl.add(table, point, 1)
    };

  let each = (x, y, f) =>
    if (x < y) {
      for (n in x to y) {
        f(n);
      };
    } else if (x > y) {
      for (n in x downto y) {
        f(n);
      };
    };

  let rec run = () =>
    try({
      let (x, y, x', y') = parse(input_line(ic));
      if (x == x') {
        each(y, y', y => incr((x, y)));
      } else if (y == y') {
        each(x, x', x => incr((x, y)));
      } else {
        diagonals := [(x, y, x', y'), ...diagonals^];
      };
      run();
    }) {
    | End_of_file =>
      close_in(ic);
      least^;
    };

  run() |> print_int;
};
