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

let rec parse_array = list => {
  let line = string => {
    let n = String.length(string);
    let array = Array.make(n, 0);
    for (i in 0 to n - 1) {
      array[i] = int_of_string(String.make(1, string.[i]));
    };
    array;
  };

  switch (list) {
  | [] => []
  | [hd, ...tl] => [line(hd), ...parse_array(tl)]
  };
};

let get = array => {
  let n = Array.length(array);

  let p = Array.length(array[0]);

  let score = ref(0);

  for (i in 0 to n - 1) {
    for (j in 0 to p - 1) {
      let min = ref(true);
      if (i > 0) {
        min := min^ && array[i - 1][j] > array[i][j];
      };
      if (j > 0) {
        min := min^ && array[i][j - 1] > array[i][j];
      };
      if (i < n - 1) {
        min := min^ && array[i + 1][j] > array[i][j];
      };
      if (j < p - 1) {
        min := min^ && array[i][j + 1] > array[i][j];
      };
      if (min^) {
        score := score^ + 1 + array[i][j];
      };
    };
  };
  score^;
};

let () =
  read_lines("./input.txt")
  |> parse_array
  |> List.to_seq
  |> Array.of_seq
  |> get
  |> print_int;
