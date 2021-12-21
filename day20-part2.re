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

let to_char_list = s => List.init(String.length(s), String.get(s));

let list_to_dec = {
  let rec aux = acc =>
    fun
    | [] => acc
    | [h, ...t] => aux(acc * 2 + (h == '.' ? 0 : 1), t);

  aux(0);
};

let parse = l => {
  let (algorithm, image) = (List.hd(l), List.tl @@ List.tl(l));

  (to_char_list(algorithm), image |> List.map(to_char_list), '.');
};

let neighbours = ((i, j)) => [
  (i - 1, j - 1),
  (i - 1, j),
  (i - 1, j + 1),
  (i, j - 1),
  (i, j),
  (i, j + 1),
  (i + 1, j - 1),
  (i + 1, j),
  (i + 1, j + 1),
];

let get = (l, def, (i, j)) =>
  try(List.nth(List.nth(l, i), j)) {
  | _ => def
  };

let step = ((algorithm, image, rest)) => {
  let p =
    image
    |> (
      l => {
        let len = (List.length @@ List.hd(l)) + 2;
        let v_p = to_char_list @@ String.make(len, rest);
        let p_l = List.map(l => [rest, ...l] @ [rest], l);

        [v_p, ...p_l] @ [v_p];
      }
    );

  p
  |> List.mapi(i => List.mapi @@ ((j, x) => (i, j, x)))
  |> List.map(
       List.map(((i, j, _)) =>
         neighbours((i, j)) |> List.map(get(p, rest))
       ),
     )
  |> List.map(List.map(list_to_dec))
  |> List.map(List.map(List.nth(algorithm)))
  |> (
    img => (
      algorithm,
      img,
      List.nth(algorithm) @@
      list_to_dec @@
      to_char_list @@
      String.make(9, rest),
    )
  );
};

let rec apply_step = (n, x) =>
  switch (n) {
  | 0 => x
  | _ => apply_step(n - 1, step(x))
  };

let () =
  read_lines("input.txt")
  |> parse
  |> apply_step(50)
  |> (
    ((_, image, _)) =>
      image
      |> List.fold_left(
           List.fold_left((acc, x) =>
             if (x == '#') {
               acc + 1;
             } else {
               acc;
             }
           ),
           0,
         )
  )
  |> print_int;
