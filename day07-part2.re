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
  let rec range = (~i=0, ~acc=0, s): int => {
    switch (i) {
    | i when i > s =>
      Printf.printf("%d - %d\n", s, acc);
      acc;
    | _ => range(~i=succ(i), ~acc=acc + i, s)
    };
  };

  let sum = List.fold_left((+), 0, list);
  let medium = sum / List.length(list);

  let rec aux = (list, fuel) => {
    switch (list) {
    | [h, ...t] => aux(t, fuel + range(Int.abs(h - medium)))
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
