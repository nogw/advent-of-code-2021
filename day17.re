let read_lines = name => {
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

type bound = {
  low: int,
  high: int,
};

let parse = input => {
  let (xb, yb) =
    Scanf.sscanf(input, "target area: x=%d..%d, y=%d..%d", (x1, x2, y1, y2) =>
      ({low: x1, high: x2}, {low: y1, high: y2})
    );
  (xb, yb);
};

let (xb, yb) = read_lines("input.txt") |> parse;

let () =
  {
    let speed = - yb.low - 1;
    speed * (speed + 1) / 2;
  }
  |> print_int;
