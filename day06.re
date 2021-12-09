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

let () = {
  let inputs =
    read_lines("input.txt")
    |> String.split_on_char(',')
    |> List.map(int_of_string);

  let pop = Array.make(9, 0);
  List.iter(fish => pop[fish] = pop[fish] + 1, inputs);

  let rec populate = days =>
    days > 0
      ? {
        let zeroes = pop[0];
        for (n in 1 to 8) {
          pop[n - 1] = pop[n];
        };
        pop[6] = pop[6] + zeroes;
        pop[8] = zeroes;

        populate(days - 1);
      }
      : {
        Array.fold_left((+), 0, pop);
      };

  populate(80) |> prerr_int;
};
