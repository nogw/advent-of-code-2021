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

type snail =
  | Base(int)
  | Snail(snail, snail);

let to_char_list = s => List.init(String.length(s), String.get(s));

let parse = l => {
  let find = s =>
    to_char_list(s)
    |> List.mapi((i, c) => (i, c))
    |> List.fold_left(
         ((count, i), (curr_index, c)) =>
           switch (c) {
           | '[' => (count + 1, i)
           | ']' => (count - 1, i)
           | ',' =>
             if (count == 0 && i == (-1)) {
               (count, curr_index);
             } else {
               (count, i);
             }
           | _ => (count, i)
           },
         (0, (-1)),
       )
    |> (((_, y)) => y);
  let rec parse_s = s =>
    switch (String.length(s)) {
    | 1 => Base(int_of_string(s))
    | n =>
      String.sub(s, 1, n - 2)
      |> (
        s => {
          let i = find(s);
          let fst = String.sub(s, 0, i);
          let snd = String.sub(s, i + 1, String.length(s) - i - 1);

          Snail(parse_s(fst), parse_s(snd));
        }
      )
    };

  l |> List.map(parse_s);
};

let rec eq = (s1, s2) => {
  switch (s1) {
  | Base(v) =>
    switch (s2) {
    | Base(v') => v == v'
    | _ => false
    }
  | Snail(s1', s2') =>
    switch (s2) {
    | Base(_) => false
    | Snail(s1'', s2'') => eq(s1', s1'') && eq(s2', s2'')
    }
  };
};

let rec explode = (num, s) => {
  let rec add = num =>
    fun
    | Base(x) => Base(x + num)
    | Snail(s1, s2) => Snail(add(num, s1), s2);
  let rec add' = num =>
    fun
    | Base(x) => Base(x + num)
    | Snail(s1, s2) => Snail(s1, add'(num, s2));

  switch (num) {
  | 0 =>
    switch (s) {
    | Snail(Base(x), Base(y)) => (x, Base(0), y)
    | _ => (0, s, 0)
    }
  | _ =>
    switch (s) {
    | Snail(s1, s2) =>
      let (left, new_s, right) = explode(num - 1, s1);

      if (eq(s1, new_s)) {
        let (left, new_s', right) = explode(num - 1, s2);

        (0, Snail(add'(left, s1), new_s'), right);
      } else {
        (left, Snail(new_s, add(right, s2)), 0);
      };
    | _ => (0, s, 0)
    }
  };
};

let rec split = s =>
  switch (s) {
  | Base(n) when n < 10 => s
  | Base(n) when n mod 2 == 0 => Snail(Base(n / 2), Base(n / 2))
  | Base(n) => Snail(Base(n / 2), Base(n / 2 + 1))
  | Snail(s1, s2) =>
    let new_s = split(s1);

    if (eq(s1, new_s)) {
      Snail(s1, split(s2));
    } else {
      Snail(new_s, s2);
    };
  };

let reduce = s => {
  let rec aux = s => {
    let (_, e, _) = explode(4, s);

    if (eq(s, e)) {
      let sp = split(s);

      if (eq(sp, s)) {
        s;
      } else {
        aux(sp);
      };
    } else {
      aux(e);
    };
  };

  aux(s);
};

let add = (s1, s2) => reduce @@ Snail(s1, s2);

let rec magnitude =
  fun
  | Base(x) => x
  | Snail(s1, s2) => 3 * magnitude(s1) + 2 * magnitude(s2);

let () =
  read_lines("input.txt")
  |> parse
  |> (
    l => List.fold_left((acc, x) => add(acc, x), List.hd(l), List.tl(l))
  )
  |> magnitude
  |> print_int;
