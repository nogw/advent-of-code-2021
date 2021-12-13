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

let rec trim = list => {
  switch (list) {
  | [] => []
  | [h, ...t] when String.length(h) == 0 => trim(t)
  | [h, ...t] => [h, ...trim(t)]
  };
};

let rec process' = list => {
  switch (list) {
  | [] => 0
  | [h, ...t] when List.mem(String.length(h), [2, 3, 4, 7]) =>
    1 + process'(t)
  | [_, ...t] => process'(t)
  };
};

let rec process = list => {
  switch (list) {
  | [] => 0
  | [h, ...t] => process'(h) + process(t)
  };
};

let () = {
  read_lines("./input.txt")
  |> List.map(String.split_on_char('|'))
  |> List.map(x => List.hd(List.tl(x)))
  |> List.map(String.split_on_char(' '))
  |> List.map(trim)
  |> process
  |> print_int;
};
