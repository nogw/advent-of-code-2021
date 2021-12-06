let read_lines = (name): list(string) => {
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

module BinMap = {
  include Map.Make(Int);

  let find = (~default, key, t) =>
    Option.value(~default) @@ find_opt(key, t);

  let add = (~default, f, key, t) =>
    add(key, f @@ find(~default, key, t), t);
};

let count_line = (map, line) =>
  line
  |> String.to_seqi
  |> Seq.fold_left(
       (map, (i, c)) =>
         BinMap.add(
           ~default=0,
           (+)(
             if (c == '1') {
               1;
             } else {
               0;
             },
           ),
           i,
           map,
         ),
       map,
     );

let () = {
  let lines = read_lines("input.txt");
  let (map, count) =
    List.fold_left(
      ((map, count), line) => (count_line(map, line), count + 1),
      (BinMap.empty, 0),
      lines,
    );

  let gamma =
    BinMap.fold(
      (_, value, acc) =>
        acc
        lsl 1
        lor (
          if (value > count / 2) {
            1;
          } else {
            0;
          }
        ),
      map,
      0,
    );

  let epsilon = {
    // or repeat gamma code, and change if (value > count / 2) to if (value < count / 2)
    let (size, _) = BinMap.max_binding(map);
    let m = lnot(0) lxor lnot(1) lsl size;
    lnot(gamma) land m;
  };

  Printf.printf("%d", gamma * epsilon);
};
