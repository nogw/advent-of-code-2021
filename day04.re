// (libraries str)) in dune file
// execute, paste input
// press CTRL + C to exec

let range = n => {
  let rec aux = (list, index) =>
    if (index == n) {
      list;
    } else {
      [index, ...aux(list, succ(index))];
    };

  aux([], 0);
};

module Numbers = {
  include Set.Make(Int);

  let drawn = (t, number) => {
    mem(number, t);
  };
};

module Board = {
  type t = array(array(int));

  let create = list => {
    list |> List.map(Array.of_list) |> Array.of_list;
  };

  let win = (numbers, board) => {
    let row = Array.exists(Array.for_all(Numbers.drawn(numbers)), board);
    let column =
      List.exists(
        i =>
          board
          |> Array.map(row => row[i])
          |> Array.for_all(Numbers.drawn(numbers)),
        range(5),
      );

    column || row;
  };

  let find_winner = (boards, numbers) => {
    List.find_opt(win(numbers), boards);
  };
};

module Bingo = {
  type t = {
    last_n: int,
    drawn: Numbers.t,
    winner: Board.t,
  };

  let find_winner = (boards, numbers) => {
    let rec aux = (boards, drawn, queue) => {
      let (last_n, drawn, queue) =
        switch (queue) {
        | [h, ...tail] => (h, Numbers.add(h, drawn), tail)
        | [] => assert(false)
        };

      switch (Board.find_winner(boards, drawn)) {
      | Some(board) => {last_n, drawn, winner: board}
      | None => aux(boards, drawn, queue)
      };
    };
    aux(boards, Numbers.empty, numbers);
  };

  let score = t => {
    t.winner
    |> Array.map(Array.to_list)
    |> Array.to_list
    |> List.flatten
    |> List.filter(n => !Numbers.drawn(t.drawn, n))
    |> List.fold_left((+), 0)
    |> ( * )(t.last_n);
  };
};

let read_numbers = () => {
  let line = read_line();
  line |> String.split_on_char(',') |> List.map(int_of_string);
};

let get_board = () => {
  assert(read_line() == "");

  List.map(
    _ =>
      read_line()
      |> Str.split(Str.regexp("[ ]+"))
      |> List.map(int_of_string),
    range(5),
  );
};

let get_boards = () => {
  let rec aux = boards =>
    try(aux([get_board(), ...boards])) {
    | End_of_file => boards
    };

  List.map(Board.create) @@ List.rev @@ aux([]);
};

let () = {
  let numbers = read_numbers();
  let boards = get_boards();
  let bingo = Bingo.find_winner(boards, numbers);
  Printf.printf("%d\n", Bingo.score(bingo));
};
