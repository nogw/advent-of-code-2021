let read_lines = (name): string => {
  let ic = open_in(name);
  let try_read = () =>
    try(Some(input_line(ic))) {
    | End_of_file => None
    };
  let rec loop = acc =>
    switch (try_read()) {
    | Some(s) => loop(s ++ acc)
    | None =>
      close_in(ic);
      acc;
    };
  loop("");
};

let crab_position = list => {
  let sort = List.sort(compare, list);

  let rec aux = (list, fuel) => {
    switch (list) {
    | [h, ...t] =>
      aux(t, fuel + Int.abs(h - List.nth(sort, List.length(sort) / 2)))
    | [] => fuel
    };
  };

  aux(list, 0);
};

let () =
  read_lines("input.txt")
  |> String.split_on_char(',')
  |> List.map(int_of_string)
  |> crab_position
  |> print_int;
