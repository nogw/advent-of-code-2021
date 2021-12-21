type pair = (elm, elm)

and elm =
  | Literal(int)
  | Pair(pair);

type stack_el =
  | Stack_l(char)
  | Stack_p(pair)
  | Int(int);

type reduction =
  | Explode(int, int)
  | Nop;

let is_digit = ch => ch >= '0' && ch <= '9';
let parse = ch => Char.code(ch) - Char.code('0');

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

let make =
  fun
  | [Int(y), Int(x), Stack_l('['), ...tl] => [
      Stack_p((Literal(x), Literal(y))),
      ...tl,
    ]
  | [Int(y), Stack_p(p), Stack_l('['), ...tl] => [
      Stack_p((Pair(p), Literal(y))),
      ...tl,
    ]
  | [Stack_p(p), Int(x), Stack_l('['), ...tl] => [
      Stack_p((Literal(x), Pair(p))),
      ...tl,
    ]
  | [Stack_p(p2), Stack_p(p1), Stack_l('['), ...tl] => [
      Stack_p((Pair(p1), Pair(p2))),
      ...tl,
    ]
  | _ => failwith("uh, bad ;/");

let pop =
  fun
  | [Stack_p(p)] => p
  | _ => failwith("uh, bad ;/");

let parse = input => {
  let rec parse' = (stack, idx) =>
    if (idx == String.length(input)) {
      stack;
    } else {
      let ch = input.[idx];
      switch (ch) {
      | '[' => parse'([Stack_l(ch), ...stack], idx + 1)
      | ch when is_digit(ch) =>
        parse'([Int(parse(ch)), ...stack], idx + 1)
      | ']' => parse'(make(stack), idx + 1)
      | _ => parse'(stack, idx + 1)
      };
    };

  parse'([], 0) |> pop;
};

let rec add_left = v =>
  fun
  | Literal(x) => Literal(x + v)
  | Pair((left, right)) => Pair((add_left(v, left), right));

let rec add_right = v =>
  fun
  | Literal(x) => Literal(x + v)
  | Pair((left, right)) => Pair((left, add_right(v, right)));

let rec reduce = ((e1, e2)) => {
  let rec split =
    fun
    | Literal(v) when v > 9 => {
        let left = v / 2;
        let right = float_of_int(v) /. 2. |> ceil |> int_of_float;
        (Pair((Literal(left), Literal(right))), true);
      }
    | Literal(_) as e => (e, false)
    | Pair((e1, e2)) => {
        let (left, splitted) = split(e1);
        if (splitted) {
          (Pair((left, e2)), true);
        } else {
          let (right, splitted) = split(e2);
          (Pair((left, right)), splitted);
        };
      };

  let rec explode = depth =>
    fun
    | Literal(_) as e => (e, Nop, false)
    | Pair((Literal(x), Literal(y))) when depth >= 4 => (
        Literal(0),
        Explode(x, y),
        true,
      )
    | Pair((e1, e2)) =>
      switch (explode(depth + 1, e1)) {
      | (left, Explode(carry, v), true) => (
          Pair((left, add_left(v, e2))),
          Explode(carry, 0),
          true,
        )
      | (left, Nop, true) => (Pair((left, e2)), Nop, true)
      | (_, _, false) =>
        switch (explode(depth + 1, e2)) {
        | (right, Explode(v, carry), true) => (
            Pair((add_right(v, e1), right)),
            Explode(0, carry),
            true,
          )
        | (right, Nop, true) => (Pair((e1, right)), Nop, true)
        | (_, op, false) => (Pair((e1, e2)), op, false)
        }
      };

  switch (explode(0, Pair((e1, e2)))) {
  | (Literal(_), _, true) => failwith("uh, bad ;/")
  | (Pair((e1, e2)), _, true) => reduce((e1, e2))
  | (e, _, false) =>
    switch (split(e)) {
    | (Literal(_), true) => failwith("uh, bad ;/")
    | (Pair((e1, e2)), true) => reduce((e1, e2))
    | (_, false) => e
    }
  };
};

let rec magnitude =
  fun
  | Literal(v) => v
  | Pair((e1, e2)) => 3 * magnitude(e1) + 2 * magnitude(e2);

let rec to_string =
  fun
  | Literal(v) => Int.to_string(v)
  | Pair((e1, e2)) =>
    Printf.sprintf("[%s,%s]", to_string(e1), to_string(e2));

let add = (pair1, pair2) => reduce((pair1, pair2));

let pairs = el => {
  let s_el = to_string(el);
  let rec pairs' = acc =>
    fun
    | [] => acc
    | [hd, ...tl] when to_string(hd) == s_el => pairs'(acc, tl)
    | [hd, ...tl] => pairs'([(el, hd), (hd, el), ...acc], tl);

  pairs'([]);
};

let sum =
  fun
  | [] => failwith("uh, bad ;/")
  | [solo] => solo
  | [first, ...tl] => List.fold_left((acc, p) => add(acc, p), first, tl);

let () = {
  read_lines("input.txt")
  |> List.map(parse)
  |> List.map(reduce)
  |> (s => s |> List.concat_map(snailfish => pairs(snailfish, s)))
  |> List.fold_left(
       (acc, (f1, f2)) => {max(add(f1, f2) |> magnitude, acc)},
       min_int,
     )
  |> print_int;
};
