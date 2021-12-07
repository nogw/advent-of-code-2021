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

let rec pow = a =>
  fun
  | 0 => 1
  | 1 => a
  | n => {
      let b = pow(a, n / 2);
      b
      * b
      * (
        if (n mod 2 == 0) {
          1;
        } else {
          a;
        }
      );
    };

let expl = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [s.[i], ...l]);
    };
  exp(String.length(s) - 1, []);
};

let int_of_binary_string = s => {
  let rec process = (chars, acc, exp) =>
    switch (chars) {
    | [c, ...cs] =>
      process(
        cs,
        acc
        + pow(2, exp)
        * (
          if (c == '1') {
            1;
          } else {
            0;
          }
        ),
        exp + 1,
      )
    | [] => acc
    };
  process(List.rev(expl(s)), 0, 0);
};

let str_lines = read_lines("input.txt");
let num_digits = String.length(List.hd(str_lines));
let int_lines = List.map(int_of_binary_string, str_lines);

let count_ones = (list, pos) => {
  let one_or_zero = (exp, x) =>
    if (x land pow(2, exp) > 0) {
      1;
    } else {
      0;
    };
  List.fold_left((acc, x) => acc + x, 0, List.map(one_or_zero(pos), list));
};

let digit_to_keep = (list, pos) => {
  let maj = float(List.length(list)) /. 2.;
  if (float(count_ones(list, pos)) >= maj) {
    1;
  } else {
    0;
  };
};

let digit_to_keep_inv = (list, pos) => {
  let maj = float(List.length(list)) /. 2.;
  if (float(count_ones(list, pos)) >= maj) {
    0;
  } else {
    1;
  };
};

let rec f = (list, pos, compare_fun) => {
  let digit = compare_fun(list, pos);
  switch (list) {
  | [x] => x
  | [_, ..._] =>
    let new_list =
      List.filter(
        x =>
          if (digit == 1) {
            x land pow(2, pos) != 0;
          } else {
            x land pow(2, pos) == 0;
          },
        list,
      );
    f(new_list, pos - 1, compare_fun);
  | _ => 0
  };
};

let o = f(int_lines, num_digits - 1, digit_to_keep);
let co2 = f(int_lines, num_digits - 1, digit_to_keep_inv);
let p2 = o * co2;

print_int(p2);
