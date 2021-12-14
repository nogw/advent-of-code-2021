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

type fold =
  | Up(int)
  | Left(int);

let tuple_of_list = list => {
  (List.nth(list, 0), List.nth(list, 1));
};

let split_string = (str, ~on) =>
  String.(
    switch (index_opt(str, on)) {
    | Some(pos) => (
        sub(str, 0, pos),
        sub(str, pos + 1, length(str) - pos - 1),
      )
    | None => exit(1)
    }
  );

let fold_of_string = s => {
  let (direction, count) = split_string(~on='=', s);
  let count' = int_of_string(count);
  switch (direction) {
  | "x" => Left(count')
  | "y" => Up(count')
  | _ => failwith("uuh bad")
  };
};

let split_while = (xs, ~f) => {
  let rec loop = acc =>
    fun
    | [hd, ...tl] when f(hd) => loop([hd, ...acc], tl)
    | t => (List.rev(acc), t);

  loop([], xs);
};

let (points, folds) = {
  let (point_lines, fold_lines) =
    read_lines("input.txt") |> split_while(~f=x => String.length(x) > 0);

  let fold_lines = List.tl(fold_lines);
  (
    point_lines
    |> List.map(line =>
         String.split_on_char(',', line)
         |> List.map(int_of_string)
         |> tuple_of_list
       ),
    fold_lines
    |> List.map(line =>
         String.split_on_char(' ', line)
         |> (l => List.nth(l, 2) |> fold_of_string)
       ),
  );
};

let reflect = (fold, (x, y)) => {
  switch (fold) {
  | Up(v) => y < v ? (x, y) : (x, v - abs(v - y))
  | Left(v) => x < v ? (x, y) : (v - abs(v - x), y)
  };
};

let remove_duplicates = (~keep=`Last, list, ~equal) => {
  let rec loop = (to_keep, accum) =>
    fun
    | [] => [to_keep, ...accum]
    | [hd, ...tl] =>
      if (equal(hd, to_keep)) {
        let to_keep =
          switch (keep) {
          | `First => to_keep
          | `Last => hd
          };

        loop(to_keep, accum, tl);
      } else {
        loop(hd, [to_keep, ...accum], tl);
      };

  switch (list) {
  | [] => []
  | [hd, ...tl] => List.rev(loop(hd, [], tl))
  };
};

let sort = (list, ~compare) =>
  switch (list) {
  | []
  | [_] => list
  | _ =>
    let equal = (x, x') => compare(x, x') == 0;
    let sorted = List.sort(compare, list);
    remove_duplicates(~equal, sorted);
  };

let compare_tuple = ((x1, y1), (x2, y2)) =>
  x1 == x2 ? compare(y1, y2) : compare(x1, x2);

let render_points = (max_x, max_y, points) => {
  let paper = Array.init(max_y, _ => Array.make(max_x, "."));
  Array.iter(((x, y)) => paper[y][x] = "#", points);
  String.concat(
    "\n",
    Array.map(row => String.concat("", Array.to_list(row)), paper)
    |> Array.to_list,
  );
};

let f = (fold, points) => {
  points |> List.map(reflect(fold)) |> sort(~compare=compare_tuple);
};

let () =
  List.fold_left((points, fold) => f(fold, points), points, folds)
  |> Array.of_list
  |> render_points(100, 100)
  |> print_endline;
