module Set = Set.Make(Char);

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

let part = line => {
  let part_part = input =>
    String.trim(input)
    |> String.split_on_char(' ')
    |> List.map(s => String.to_seq(s) |> Set.of_seq);

  String.split_on_char('|', line) |> List.map(part_part);
};

let solve_line = line => {
  let build = s => {
    let decoding = Hashtbl.create(10);

    let arr = Array.make(10, Set.empty);

    let assoc = (set, digit) => {
      Hashtbl.replace(decoding, Set.elements(set), digit);
      arr[digit] = set;
    };

    let encode = digit => arr[digit];

    let decode = set => Hashtbl.find(decoding, Set.elements(set));

    let rec phase = input =>
      switch (input) {
      | [] => []
      | [set, ...tail] =>
        switch (Set.cardinal(set)) {
        | 2 =>
          assoc(set, 1);
          phase(tail);
        | 3 =>
          assoc(set, 7);
          phase(tail);
        | 4 =>
          assoc(set, 4);
          phase(tail);
        | 7 =>
          assoc(set, 8);
          phase(tail);
        | _ => [set, ...phase(tail)]
        }
      };

    let rec phase' = input =>
      switch (input) {
      | [] => []
      | [set, ...tail] =>
        switch (Set.cardinal(set)) {
        | 5 =>
          if (Set.subset(encode(1), set)) {
            assoc(set, 3);
            phase'(tail);
          } else {
            [set, ...phase'(tail)];
          }
        | 6 =>
          if (Bool.(!)(Set.subset(encode(1), set))) {
            assoc(set, 6);
            phase'(tail);
          } else {
            [set, ...phase'(tail)];
          }
        | _ => [set, ...phase'(tail)]
        }
      };

    let rec phase'' = input =>
      switch (input) {
      | [] => []
      | [set, ...tail] =>
        switch (Set.cardinal(set)) {
        | 5 =>
          if (Set.subset(set, encode(6))) {
            assoc(set, 5);
            phase''(tail);
          } else {
            [set, ...phase''(tail)];
          }
        | 6 =>
          if (Set.subset(encode(3), set)) {
            assoc(set, 9);
            phase''(tail);
          } else {
            [set, ...phase''(tail)];
          }
        | _ => [set, ...phase''(tail)]
        }
      };

    let rec phase''' = input =>
      switch (input) {
      | [] => []
      | [set, ...tail] =>
        switch (Set.cardinal(set)) {
        | 5 =>
          assoc(set, 2);
          phase'''(tail);
        | 6 =>
          assoc(set, 0);
          phase'''(tail);
        | _ => failwith("uuh bad")
        }
      };

    let _ = s |> phase |> phase' |> phase'' |> phase''';
    let decoder = e => List.map(decode, e);

    decoder;
  };

  let to_number = digits =>
    List.fold_left((acc, d) => acc * 10 + d, 0, digits);

  switch (part(line)) {
  | [s, e] =>
    let decoder = build(s);
    decoder(e) |> to_number;
  | _ => failwith("uuh bad")
  };
};

let () =
  read_lines("./input.txt")
  |> (
    inp => List.map(solve_line, inp) |> List.fold_left((+), 0) |> print_int
  );
