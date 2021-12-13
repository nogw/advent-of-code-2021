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

let process' = (list, incomplete) => {
  let rec aux = (list, incomplete, acc) =>
    switch (list) {
    | [] => [acc, ...incomplete]
    | [h, ...t] =>
      switch (h) {
      | '['
      | '('
      | '{'
      | '<' => aux(t, incomplete, [h, ...acc])
      | ']'
      | ')'
      | '}'
      | '>' =>
        switch (h, List.hd(acc)) {
        | (']', '[')
        | (')', '(')
        | ('}', '{')
        | ('>', '<') => aux(t, incomplete, List.tl(acc))
        | (_, _) => incomplete
        }
      | _ => failwith("uuh bad")
      }
    };

  aux(list, incomplete, []);
};

let process = list => {
  let rec p_aux = (list, incomplete) =>
    switch (list) {
    | [] => incomplete
    | [hd, ...tl] =>
      let incomplete' = process'(hd, incomplete);
      p_aux(tl, incomplete');
    };

  p_aux(list, []);
};

let explode = s => {
  let rec aux = (idx, line) =>
    idx < 0 ? line : aux(pred(idx), [s.[idx], ...line]);
  aux(String.length(s) - 1, []);
};

let score = char =>
  switch (char) {
  | '(' => 1
  | '[' => 2
  | '{' => 3
  | '<' => 4
  | _ => 0
  };

let score' = list => {
  let rec cs_aux = (list, scr) =>
    switch (list) {
    | [] => scr
    | [hd, ...tl] => cs_aux(tl, scr * 5 + score(hd))
    };
  cs_aux(list, 0);
};

let () = {
  let final =
    List.map(explode, read_lines("./input.txt"))
    |> process
    |> (error => Array.of_seq(List.to_seq(List.map(score', error))));

  Array.sort(Int.compare, final);
  final[Array.length(final) / 2] |> print_int;
};
