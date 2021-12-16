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

let to_matrix = lines => {
  let aux = lines' => {
    let l = Array.length(lines');
    let l' = String.length(lines'[0]);
    Array.init(l, i =>
      Array.init(
        l',
        j => {
          let s = String.sub(lines'[i], j, 1);
          try(int_of_string(s)) {
          | Failure(e) => failwith @@ e ++ " : " ++ s
          };
        },
      )
    );
  };

  lines |> Array.of_list |> aux;
};

exception Finish(array(array(int)));

let matrix0 = (ybound, xbound, matrix') => {
  let a = Array.make_matrix(5 * (ybound + 1), 5 * (xbound + 1), -1);
  for (b in 0 to 4) {
    for (c in 0 to 4) {
      for (d in 0 to xbound) {
        for (e in 0 to ybound) {
          a[b * (ybound + 1) + e][c * (xbound + 1) + d] = {
            let w = matrix'[e][d] + b + c;
            w mod 10 + w / 10;
          };
        };
      };
    };
  };
  a;
};

let neighbours = (~allow_diags=true, xbound, ybound, p) => {
  let cop = (op, (x, y), (a, b)) => (op(x, a), op(y, b));

  let b = (xbound, ybound, (x, y)) => {
    let bound = (b, coord) => 0 <= coord && coord <= b;
    bound(xbound, x) && bound(ybound, y);
  };

  let bound = b(xbound, ybound);
  let (++) = cop((+));
  List.filter(bound) @@
  List.map(
    x => x ++ p,
    (
      if (allow_diags) {
        [(1, 1), (1, (-1)), ((-1), 1), ((-1), (-1))];
      } else {
        [];
      }
    )
    @ [(0, 1), (0, (-1)), (1, 0), ((-1), 0)],
  );
};

let map_matrix = (f, mat) => Array.map(arr => Array.map(f, arr), mat);

let iteri_matrix = f =>
  Array.iteri((y, arr) => Array.iteri((x, e) => f(y, x, e), arr));

exception Empty;

module Make = (X: {
                 type t;
                 let compare: (t, t) => int;
               }) => {
  type t = {
    mutable size: int,
    mutable data: array(X.t),
    x: X.t,
    min_cap: int,
  };

  let create = (~x, n) => {
    if (n < 0 || n > Sys.max_array_length) {
      invalid_arg("create");
    };
    let n = max(16, n);
    {size: 0, data: Array.make(n, x), x, min_cap: n};
  };

  let length = h => h.size;

  let is_empty = h => h.size == 0;

  let enlarge = h => {
    let n = h.size;
    let n' = min(2 * n, Sys.max_array_length);
    if (n' == n) {
      failwith("maximum capacity reached");
    };
    let d = h.data;
    let d' = Array.make(n', h.x);
    Array.blit(d, 0, d', 0, n);
    h.data = d';
  };

  let shrink = h => {
    let n = Array.length(h.data);
    let n' = max(h.min_cap, n / 2);
    if (n' < n) {
      let d = h.data;
      let d' = Array.make(n', h.x);
      Array.blit(d, 0, d', 0, h.size);
      h.data = d';
    };
  };

  let add = (h, x) => {
    let n = h.size;
    if (n === Array.length(h.data)) {
      enlarge(h);
    };
    let d = h.data;
    let rec moveup = i => {
      let fi = (i - 1) / 2;
      if (i > 0 && X.compare(d[fi], x) > 0) {
        d[i] = d[fi];
        moveup(fi);
      } else {
        d[i] = x;
      };
    };

    moveup(n);
    h.size = n + 1;
  };

  let minimum = h => {
    if (h.size <= 0) {
      raise(Empty);
    };
    h.data[0];
  };

  let rec movedown = (d, n, i, x) => {
    let j = 2 * i + 1;
    if (j < n) {
      let j = {
        let j' = j + 1;
        if (j' < n && X.compare(d[j'], d[j]) < 0) {
          j';
        } else {
          j;
        };
      };

      if (X.compare(d[j], x) < 0) {
        d[i] = d[j];
        movedown(d, n, j, x);
      } else {
        d[i] = x;
      };
    } else {
      d[i] = x;
    };
  };

  let remove = h => {
    if (h.size <= 0) {
      raise(Empty);
    };
    let n = h.size - 1;
    h.size = n;
    let d = h.data;
    let x = d[n];
    d[n] = h.x;
    movedown(d, n, 0, x);
    if (4 * h.size < Array.length(h.data)) {
      shrink(h);
    };
  };

  let pop_minimum = h => {
    let m = minimum(h);
    remove(h);
    m;
  };
};

let path = (xbound, ybound, matrix, end_point) => {
  let neighbours = neighbours(~allow_diags=false, xbound, ybound);

  let cost = ((x, y)) => matrix[y][x];

  let path_costs = map_matrix(_ => 10 * (xbound + ybound), matrix);

  path_costs[0][0] = 0;

  let path_cost = ((x, y)) => path_costs[y][x];

  let update_cost = ((x, y), cost) => path_costs[y][x] = cost;

  module Heap =
    Make({
      type t = (int, int);
      let compare = (p1, p2) => compare(path_cost(p1), path_cost(p2));
    });

  let heap = Heap.create(~x=((-1), (-1))) @@ 2 * xbound * ybound;

  iteri_matrix((y, x, _) => Heap.add(heap, (x, y)), path_costs);

  let (x, y) = Heap.minimum(heap);

  Printf.printf("size : %d, first : %d,%d %!\n", Heap.length(heap), x, y);

  while ((!) @@ Heap.is_empty(heap)) {
    let p = Heap.pop_minimum(heap);
    List.iter(neighbour => {
      let tot_cost = path_cost(p) + cost(neighbour);
      if (path_cost(neighbour) > tot_cost) {
        update_cost(neighbour, tot_cost);
        Heap.add(heap, neighbour);
      };
    }) @@
    neighbours(p);
  };
  (path_cost(end_point), path_costs);
};

let _ = {
  let matrix = read_lines("input.txt") |> to_matrix;
  let ybound = pred(Array.length(matrix));
  let xbound = pred(Array.length(matrix[0]));

  let (r, _) = path(xbound, ybound, matrix, (xbound, ybound));
  r |> print_int;
};
