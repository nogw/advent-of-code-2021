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

module Step = {
  type t =
    | Forward(int)
    | Down(int)
    | Up(int);

  let of_string = (line: string) => {
    let step_of_string = (step: string, value: int) => {
      switch (step) {
      | "forward" => Forward(value)
      | "down" => Down(value)
      | "up" => Up(value)
      | invalid => failwith("invalid step " ++ invalid)
      };
    };

    switch (String.split_on_char(' ', line)) {
    | [m, v, ..._] => step_of_string(m, int_of_string(v))
    | _ => failwith("wrong line format")
    };
  };
};

module Move = {
  type t = {
    horizontal: int,
    depth: int,
  };

  let move = (p, step) => {
    Step.(
      switch (step) {
      | Forward(v) => {...p, horizontal: p.horizontal + v}
      | Down(v) => {...p, depth: p.depth + v}
      | Up(v) => {...p, depth: p.depth - v}
      }
    );
  };

  let mult = v => {
    v.horizontal * v.depth;
  };
};

let _ =
  read_lines("input.txt")
  |> List.map(Step.of_string)
  |> List.fold_left(Move.move, {horizontal: 0, depth: 0})
  |> Move.mult
  |> print_int;
