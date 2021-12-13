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

let process' = (list, corrupted, incomplete) => {
  let rec aux = (list, corrupted, incomplete, acc) => {
    switch (list) {
    | [] => (corrupted, [list, ...incomplete])
    | [h, ...t] =>
      switch (h) {
      | '['
      | '('
      | '{'
      | '<' => aux(t, corrupted, incomplete, [h, ...acc])
      | ']'
      | ')'
      | '}'
      | '>' =>
        switch (h, List.hd(acc)) {
        | (']', '[')
        | (')', '(')
        | ('}', '{')
        | ('>', '<') => aux(t, corrupted, incomplete, List.tl(acc))
        | (_, _) => ([list, ...corrupted], incomplete)
        }
      | _ => failwith("uuh bad")
      }
    };
  };

  aux(list, corrupted, incomplete, []);
};

let process = list => {
  let rec aux = (list, corrupted, incomplete) => {
    switch (list) {
    | [] => corrupted
    | [h, ...t] =>
      let (corruped', incomplete') = process'(h, corrupted, incomplete);
      aux(t, corruped', incomplete');
    };
  };

  aux(list, [], []);
};

let explode = s => {
  let rec aux = (idx, line) =>
    idx < 0 ? line : aux(pred(idx), [s.[idx], ...line]);
  aux(String.length(s) - 1, []);
};

let score = v => {
  switch (v) {
  | ')' => 3
  | ']' => 57
  | '}' => 1197
  | '>' => 25137
  | _ => 0
  };
};

let () = {
  read_lines("./input.txt")
  |> List.map(explode)
  |> (
    list =>
      List.map(List.hd, process(list))
      |> (list => List.fold_left((+), 0, List.map(score, list)) |> print_int)
  );
};
