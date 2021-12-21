let split = (s, l) => {
  let rec aux = (acc, acc_list) =>
    fun
    | [] => acc_list @ [acc]
    | [hd, ...tl] when hd == s => aux([], acc_list @ [acc], tl)
    | [hd, ...tl] => aux(acc @ [hd], acc_list, tl);

  aux([], [], l);
};

let parse = l =>
  l
  |> split("")
  |> List.map @@
  List.tl
  |> List.map(
       List.map(s =>
         String.split_on_char(',', s)
         |> List.map(int_of_string)
         |> (
           l => (
             List.hd(l),
             List.hd @@ List.tl(l),
             List.hd @@ List.tl @@ List.tl(l),
           )
         )
       ),
     );

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
      List.rev(acc) |> parse;
    };
  loop([]);
};

let rec fold = (f, p, acc) =>
  fun
  | [_, ..._] when p(acc) => acc
  | [x, ...xs] => fold(f, p, f(acc, x), xs)
  | [] => acc;

let orientations = ((x, y, z)) =>
  [
    (1, 1, 1),
    ((-1), 1, 1),
    (1, (-1), 1),
    (1, 1, (-1)),
    ((-1), (-1), 1),
    ((-1), 1, (-1)),
    (1, (-1), (-1)),
    ((-1), (-1), (-1)),
  ]
  |> List.map @@
  (((x1, y1, z1)) => (x * x1, y * y1, z * z1));

let rotations = ((x, y, z)) => [
  (x, y, z),
  (x, z, y),
  (y, x, z),
  (y, z, x),
  (z, x, y),
  (z, y, x),
];

let orient_and_rotate = x => orientations(x) |> List.concat_map(rotations);

let rec to' =
  fun
  | []
  | [[], ..._] => []
  | rows => [List.map(List.hd, rows), ...to'(List.map(List.tl, rows))];

let all_directions = l => l |> List.map(orient_and_rotate) |> to';

let scan = ((x, y, z)) =>
  List.map(((x1, y1, z1)) => (x + x1, y + y1, z + z1));

let point = ((x1, y1, z1), (x2, y2, z2)) =>
  if (x1 == x2) {
    if (y1 == y2) {
      z1 - z2;
    } else {
      y1 - y2;
    };
  } else {
    x1 - x2;
  };

let points = (fixed, points) =>
  all_directions(points)
  |> fold(
       ((acc_s, acc_p), points) => {
         let possible_scanners =
           List.concat_map(
             ((x, y, z)) =>
               fixed |> List.map(((x1, y1, z1)) => (x1 - x, y1 - y, z1 - z)),
             points,
           );

         possible_scanners
         |> fold(
              ((acc_s, acc_p), scanner) => {
                let moved = scan(scanner, points);
                let count =
                  List.filter(x => List.mem(x, fixed), moved) |> List.length;

                if (count >= 12) {
                  (scanner, moved);
                } else {
                  (acc_s, acc_p);
                };
              },
              ((_, acc_p)) => List.length(acc_p) > 0,
              (acc_s, acc_p),
            );
       },
       ((_, acc_p)) => List.length(acc_p) > 0,
       ((0, 0, 0), []),
     );

let f = l => {
  let rec aux = fixed =>
    fun
    | [] => fixed
    | [hd, ...tl] => {
        let fix =
          fold(
            (_, l) => {
              let (_, p) = points(l, hd);

              p;
            },
            acc => List.length(acc) > 0,
            [],
            fixed,
          );

        if (List.length(fix) == 0) {
          aux(fixed, tl @ [hd]);
        } else {
          aux(fixed @ [fix], tl);
        };
      };

  aux([List.hd(l)], List.tl(l))
  |> List.concat
  |> List.sort_uniq(point)
  |> List.length;
};

let () = read_lines("input.txt") |> f |> print_int;
