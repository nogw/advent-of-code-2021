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

let to_char_list = s => List.init(String.length(s), String.get(s));

let rec take = (n, l) => {
  n <= 0
    ? []
    : (
      switch (l) {
      | [] => []
      | [h, ...t] => [h, ...take(pred(n), t)]
      }
    );
};

let rec drop = (n, l) => {
  n <= 0
    ? l
    : (
      switch (l) {
      | [] => []
      | [_, ...t] => drop(pred(n), t)
      }
    );
};

let split = (n, l) => (take(n, l), drop(n, l));

let hex_to_bin =
  fun
  | '0' => "0000"
  | '1' => "0001"
  | '2' => "0010"
  | '3' => "0011"
  | '4' => "0100"
  | '5' => "0101"
  | '6' => "0110"
  | '7' => "0111"
  | '8' => "1000"
  | '9' => "1001"
  | 'A' => "1010"
  | 'B' => "1011"
  | 'C' => "1100"
  | 'D' => "1101"
  | 'E' => "1110"
  | 'F' => "1111"
  | _ => "";

let char_list_to_dec = {
  let rec aux = acc =>
    fun
    | [] => acc
    | [h, ...t] => aux(acc * 2 + int_of_char(h) - 48, t);

  aux(0);
};

let parse = l => {
  List.hd(l)
  |> to_char_list
  |> List.map(hex_to_bin)
  |> List.fold_left((++), "")
  |> to_char_list;
};

type packet =
  | Operator({
      version: int,
      type_id: int,
      sub_packets: list(packet),
    })
  | Literal({
      version: int,
      value: int,
    });

// TODO: Fix warning 8
[@warning "-8"]
let rec parse_literal = (num, l) => {
  let ([c, ...bits], rest) = split(5, l);

  c == '1'
    ? parse_literal(num @ bits, rest) : (char_list_to_dec(num @ bits), rest);
};

// TODO: Fix warning 8
[@warning "-8"]
let rec parse_operator = (_, l) => {
  let ([length_type], rest) = split(1, l);

  if (length_type == '0') {
    let (length, rest) =
      split(15, rest) |> (((x, y)) => (char_list_to_dec(x), y));
    let (cur, rest) = split(length, rest);
    let rec helper = (acc, cur) =>
      if (cur == []) {
        acc;
      } else {
        let (parsed, rest) = parse_and_get_rest(cur);

        helper(acc @ [parsed], rest);
      };

    (helper([], cur), rest);
  } else {
    let (num, rest) =
      split(11, rest) |> (((x, y)) => (char_list_to_dec(x), y));
    let rec helper = (acc, n, cur) =>
      if (n == 0) {
        (acc, cur);
      } else {
        let (parsed, rest) = parse_and_get_rest(cur);

        helper(acc @ [parsed], n - 1, rest);
      };

    helper([], num, rest);
  };
}
and parse_and_get_rest = l => {
  let (version, rest) =
    split(3, l) |> (((x, y)) => (char_list_to_dec(x), y));
  let (type_id, rest) =
    split(3, rest) |> (((x, y)) => (char_list_to_dec(x), y));

  if (type_id == 4) {
    let (value, rest) = parse_literal([], rest);

    (Literal({version, value}), rest);
  } else {
    let (sub_packets, rest) = parse_operator([], rest);
    (Operator({version, type_id, sub_packets}), rest);
  };
};

let rec value =
  fun
  | Literal({value, _}) => value
  | Operator({type_id, sub_packets, _}) =>
    switch (type_id) {
    | n when 0 <= n && n <= 3 =>
      let (f, init) =
        switch (n) {
        | 0 => (((sum, x) => sum + value(x)), 0)
        | 1 => (((prod, x) => prod * value(x)), 1)
        | 2 => (
            (
              (min, x) => {
                let v = value(x);
                if (v < min) {
                  v;
                } else {
                  min;
                };
              }
            ),
            max_int,
          )
        | _ => (
            (
              (max, x) => {
                let v = value(x);
                if (v > max) {
                  v;
                } else {
                  max;
                };
              }
            ),
            0,
          )
        };

      List.fold_left(f, init, sub_packets);
    | n when 5 <= n && n <= 7 =>
      let first = value @@ List.hd(sub_packets);
      let second = value @@ List.nth(sub_packets, 1);
      let comp =
        switch (n) {
        | 5 => (>)
        | 6 => (<)
        | _ => (==)
        };

      if (comp(first, second)) {
        1;
      } else {
        0;
      };
    | _ => 0
    };

let f = x => {
  let (parsed, _) = parse_and_get_rest(x);

  value(parsed);
};

let _ = read_lines("input.txt") |> parse |> f |> print_int;
