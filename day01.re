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

let count_measurement_increases = (measurement): int => {
  let rec aux = (count, list) => {
    switch (list) {
    | [hd, hd', ...tail] =>
      let c =
        if (hd' > hd) {
          succ(count);
        } else {
          count;
        };
      aux(c, [hd', ...tail]);
    | [_]
    | [] => count
    };
  };

  aux(0, measurement);
};

let _ =
  read_lines("input.txt")
  |> List.map(int_of_string)
  |> count_measurement_increases
  |> print_int;
