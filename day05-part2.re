let parse = line => {
  let sub = (start, c) => {
    let i = String.index_from(line, start, c);
    let str = String.sub(line, start, i - start);
    (int_of_string(str), i + 1);
  };

  let (x, i) = sub(0, ',');
  let (x', i) = sub(i, ' ');
  let (x'', i) = sub(i + 3, ',');
  let d = String.sub(line, i, String.length(line) - i) |> int_of_string;

  (x, x', x'', d);
};

module PointHash = {
  type t = (int, int);
  let equal = ((a1, b1), (a2, b2)) =>
    Int.equal(a1, a2) && Int.equal(b1, b2);
  let hash = (x: (int, int)) => Hashtbl.hash(x);
};

module PointHashtbl = Hashtbl.Make(PointHash);

let () = {
  let ic = open_in("input.txt");
  let table = PointHashtbl.create(1000);
  let at_least_2 = ref(0);

  let diagonals = ref([]);

  let incr = point =>
    switch (PointHashtbl.find_opt(table, point)) {
    | Some(v) =>
      let y = v + 1;
      if (y == 2) {
        at_least_2 := at_least_2^ + 1;
      };
      PointHashtbl.replace(table, point, v + 1);

    | None => PointHashtbl.add(table, point, 1)
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

  let rec f1 = () =>
    try({
      let (x, y, x', y') = parse(input_line(ic));
      if (x == x') {
        each(y, y', y => incr((x, y)));
      } else if (y == y') {
        each(x, x', x => incr((x, y)));
      } else {
        diagonals := [(x, y, x', y'), ...diagonals^];
      };
      f1();
    }) {
    | End_of_file => close_in(ic)
    };

  f1();

  let each2 = (x, y, x', y', f) => {
    let f1 =
      if (x < x') {
        (+);
      } else {
        (-);
      };
    let f2 =
      if (y < y') {
        (+);
      } else {
        (-);
      };

    let rec each2 = (x, y) => {
      f(x, y);
      if (x != x') {
        each2(f1(x, 1), f2(y, 1));
      };
    };

    each2(x, y);
  };

  let rec f2 =
    fun
    | [(x, y, x', y'), ...rest] => {
        each2(x, y, x', y', (x, y) => incr((x, y)));
        f2(rest);
      }
    | [] => print_endline("Part 2: " ++ string_of_int(at_least_2^));

  f2(diagonals^);
};
