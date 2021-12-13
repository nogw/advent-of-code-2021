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

let pairs = (~allow=true, p, xb, yb) => {
  let couple = (op, (x, y), (a, b)) => (op(x, a), op(y, b));

  let bound = (xb, yb, (x, y)) => {
    let bound = (b, coord) => 0 <= coord && coord <= b;
    bound(xb, x) && bound(yb, y);
  };

  let bound = bound(xb, yb);

  let (++) = couple((+));
  List.filter(bound) @@
  List.map(
    x => x ++ p,
    (
      if (allow) {
        [(1, 1), (1, (-1)), ((-1), 1), ((-1), (-1))];
      } else {
        [];
      }
    )
    @ [(0, 1), (0, (-1)), (1, 0), ((-1), 0)],
  );
};

let mapi = (f, arr) =>
  for (i in 0 to Array.length(arr) - 1) {
    arr[i] = f(i, arr[i]);
  };

let process = s => {
  let queue = Queue.create();
  let upgrade = (is_conflict, y, x, s) =>
    s == 9
      ? {
        Queue.add((x, y), queue);
        0;
      }
      : {
        s
        + 1
        - max(
            0,
            if (is_conflict) {
              1 - s;
            } else {
              0;
            },
          );
      };

  Array.iteri((i, x) => mapi(upgrade(false, i), x), s);

  let rec conflicts = acc =>
    if (Queue.is_empty(queue)) {
      acc;
    } else {
      List.iter(((x, y)) => s[y][x] = (upgrade(true))(y, x, s[y][x])) @@
      pairs(Queue.take(queue), 9, 9);
      conflicts(acc + 1);
    };

  conflicts(0);
};

let rec iter = (squids, times, acc) =>
  if (times == 0) {
    acc;
  } else {
    iter(squids, times - 1, process(squids) + acc);
  };

let to_matrix = lines => {
  let l = Array.length(lines);
  let l' = String.length(lines[0]);
  Array.init(l, i =>
    Array.init(
      l',
      j => {
        let s = String.sub(lines[i], j, 1);
        try(int_of_string(s)) {
        | Failure(e) => failwith @@ e ++ " : " ++ s
        };
      },
    )
  );
};

let () =
  read_lines("./input.txt")
  |> Array.of_list
  |> to_matrix
  |> Array.map(Array.copy)
  |> (squids => iter(squids, 100, 0))
  |> print_int;
