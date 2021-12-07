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

let lines = read_lines("input.txt");

let boards_str =
  switch (lines) {
  | [_, _, ...rest] => rest
  | _ => []
  };

let rec chunk = list =>
  switch (list) {
  | [a, b, c, d, e, ...rest] => [[a, b, c, d, e], ...chunk(rest)]
  | _ => []
  };

let rec split = (char, str) =>
  try({
    let pos = String.index(str, char);
    let dir = String.sub(str, 0, pos);
    let rest = String.sub(str, pos + 1, String.length(str) - (pos + 1));
    [dir, ...split(char, rest)];
  }) {
  | Not_found => [str]
  };

let chunkedboards =
  List.map(
    nestlist => List.map(split(' '), nestlist),
    chunk(List.filter(x => x != "", boards_str)),
  );

let remove_empty_cells_from_row = row => List.filter(cell => cell != "", row);
let clean_each_row_in_board = board =>
  List.map(remove_empty_cells_from_row, board);

let apply_to_all_cell = (fn, row) => List.map(fn, row);
let apply_to_all_row = (fn, board) =>
  List.map(apply_to_all_cell(fn), board);
let apply_to_all_board = (fn, board_list) =>
  List.map(apply_to_all_row(fn), board_list);

let cleaned_boards = List.map(clean_each_row_in_board, chunkedboards);
let inted_boards = apply_to_all_board(int_of_string, cleaned_boards);

let tupled_boards = apply_to_all_board(cell => (cell, false), inted_boards);

let drawn =
  List.map(
    int_of_string,
    List.filter(str => str != "", split(',', List.hd(lines))),
  );

let mark_cell = (number, tup) => {
  let (value, _) = tup;
  if (number == value) {
    (value, true);
  } else {
    tup;
  };
};

let rec check_rows = inboard =>
  switch (inboard) {
  | [row, ...rows] =>
    if (List.for_all(
          tup => {
            let (_, marked) = tup;
            marked;
          },
          row,
        )) {
      true;
    } else {
      check_rows(rows);
    }
  | _ => false
  };

let convert = row =>
  List.map(
    tup => {
      let (_, marked) = tup;
      if (marked) {1} else {0};
    },
    row,
  );

let count_column = board =>
  List.fold_left(
    (acclist, list) => List.map2((+), acclist, list),
    [0, 0, 0, 0, 0],
    List.map(convert, board),
  );

let check_winning_column = board =>
  List.exists(x => x == 5, count_column(board));

let sum_unmarked = board =>
  List.fold_left(
    (acc, t) => {
      let (v, _) = t;
      acc + v;
    },
    0,
    List.filter(
      t => {
        let (_, m) = t;
        !m;
      },
      List.flatten(board),
    ),
  );

let winning_board = board =>
  check_winning_column(board) || check_rows(board);

let rec find_winning_board = (numbers, boards, last_number) =>
  switch (numbers) {
  | [] => (0, None)
  | _ =>
    let drawn = List.hd(numbers);
    let remaining_boards =
      if (List.length(boards) == 1) {
        boards;
      } else {
        List.map(
          opt => Option.get(opt),
          List.filter(
            opt =>
              switch (opt) {
              | None => false
              | _ => true
              },
            List.map(
              board =>
                if (winning_board(board)) {
                  None;
                } else {
                  Some(board);
                },
              boards,
            ),
          ),
        );
      };
    switch (List.exists(winning_board, remaining_boards)) {
    | true => (last_number, Some(remaining_boards))
    | _ =>
      find_winning_board(
        List.tl(numbers),
        apply_to_all_board(mark_cell(drawn), remaining_boards),
        drawn,
      )
    };
  };

let (drawn, board) = find_winning_board(drawn, tupled_boards, 0);

let () = print_int(drawn * sum_unmarked(Option.get(board) |> List.hd));
