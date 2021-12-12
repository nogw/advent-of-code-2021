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

module IntPair = {
  type t = (int, int);
  let compare = ((x0, y0), (x1, y1)) =>
    switch (Stdlib.compare(x0, x1)) {
    | 0 => Stdlib.compare(y0, y1)
    | c => c
    };
  let equals = ((x0, y0), (x1, y1)) => x0 === x1 && y0 === y1;
};

module PairSet = Set.Make(IntPair);

let rec parse_array = list => {
  let parse_one_line = string => {
    let n = String.length(string);
    let array = Array.make(n, 0);
    for (i in 0 to n - 1) {
      array[i] = int_of_string(String.make(1, string.[i]));
    };
    array;
  };

  switch (list) {
  | [] => []
  | [hd, ...tl] => [parse_one_line(hd), ...parse_array(tl)]
  };
};

let get = array => {
  let n = Array.length(array);
  let p = Array.length(array[0]);
  let indexes = ref([]);
  for (i in 0 to n - 1) {
    for (j in 0 to p - 1) {
      let minimum = ref(true);
      if (i > 0) {
        minimum := minimum^ && array[i - 1][j] > array[i][j];
      };
      if (j > 0) {
        minimum := minimum^ && array[i][j - 1] > array[i][j];
      };
      if (i < n - 1) {
        minimum := minimum^ && array[i + 1][j] > array[i][j];
      };
      if (j < p - 1) {
        minimum := minimum^ && array[i][j + 1] > array[i][j];
      };
      if (minimum^) {
        indexes := [(i, j), ...indexes^];
      };
    };
  };
  indexes^;
};

let find_basin = (array, minimum) => {
  let queue = Queue.create();
  let fill = (array, queue) => {
    let n = Array.length(array);
    let p = Array.length(array[0]);
    let basin = ref(PairSet.empty);
    let border = ref(PairSet.empty);
    while (!Queue.is_empty(queue)) {
      let (i, j) = Queue.take(queue);
      if (!(array[i][j] === 9)) {
        basin := PairSet.add((i, j), basin^);
        if (i > 0) {
          if (!(
                PairSet.mem((i - 1, j), basin^)
                || PairSet.mem((i - 1, j), border^)
              )) {
            Queue.add((i - 1, j), queue);
          };
        };
        if (j > 0) {
          if (!(
                PairSet.mem((i, j - 1), basin^)
                || PairSet.mem((i, j - 1), border^)
              )) {
            Queue.add((i, j - 1), queue);
          };
        };
        if (i < n - 1) {
          if (!(
                PairSet.mem((i + 1, j), basin^)
                || PairSet.mem((i + 1, j), border^)
              )) {
            Queue.add((i + 1, j), queue);
          };
        };
        if (j < p - 1) {
          if (!(
                PairSet.mem((i, j + 1), basin^)
                || PairSet.mem((i, j + 1), border^)
              )) {
            Queue.add((i, j + 1), queue);
          };
        };
      } else {
        border := PairSet.add((i, j), border^);
      };
    };
    PairSet.cardinal(basin^);
  };

  Queue.add(minimum, queue);
  fill(array, queue);
};

let rec basins = (array, list) =>
  switch (list) {
  | [] => []
  | [hd, ...tl] => [find_basin(array, hd), ...basins(array, tl)]
  };

let maxes = list => {
  let rec tm_aux = (list, max1, max2, max3) =>
    switch (list) {
    | [] => (max1, max2, max3, max1 * max2 * max3)
    | [hd, ...tl] =>
      if (hd > max1) {
        tm_aux(tl, hd, max1, max2);
      } else if (hd > max2) {
        tm_aux(tl, max1, hd, max2);
      } else if (hd > max3) {
        tm_aux(tl, max1, max2, hd);
      } else {
        tm_aux(tl, max1, max2, max3);
      }
    };
  tm_aux(list, 0, 0, 0);
};

let () = {
  let inp =
    "./input.txt" |> read_lines |> parse_array |> List.to_seq |> Array.of_seq;
  let (a, b, c, d) = get(inp) |> basins(inp) |> maxes;
  Printf.printf("%d, %d, %d, r: %d", a, b, c, d);
};
