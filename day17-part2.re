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

let in_bx = x => xb.low <= x && x <= xb.high;
let in_by = y => yb.low <= y && y <= yb.high;

let check = (sx, sy) => {
  let rec aux = ((x, y), sx, sy) =>
    in_bx(x)
    && in_by(y)
    || (sx >= 0 || in_bx(x))
    && y >= yb.low
    && x <= xb.high
    && aux((x + sx, y + sy), max(0) @@ sx - 1, sy - 1);
  aux((0, 0), sx, sy);
};

let run = () => {
  let r = ref(0);

  for (sy in yb.low to - yb.low - 1) {
    for (sx in 0 to xb.high) {
      if (check(sx, sy)) {
        incr(r);
      };
    };
  };

  r^;
};

let () = run() |> print_int;
