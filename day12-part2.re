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

let hashtbl = Hashtbl.create(20);

let add = (s, s') => {
  let aux = (s, sa) => {
    Hashtbl.replace(hashtbl, s) @@
    [
      sa,
      ...switch (Hashtbl.find_opt(hashtbl, s)) {
         | None => []
         | Some(l) => l
         },
    ];
  };
  aux(s, s');
  aux(s', s);
};

let extract_node = s => {
  Scanf.sscanf(s, "%[^-]-%s", (s1, s2) => add(s1, s2));
};

let debug = () =>
  Hashtbl.iter(
    (s, l) => {
      Printf.printf("\n\n%s -> ", s);
      List.iter(Printf.printf("%s, "), l);
    },
    hashtbl,
  );

let is_capitalize = s => String.capitalize_ascii(s) == s;

let get = (table, key, default) => {
  switch (Hashtbl.find_opt(table, key)) {
  | Some(x) => x
  | None => default
  };
};

let ican = (v, isp2) => {
  fun
  | "start" => false
  | s =>
    is_capitalize(s)
    || get(v, s, 0) <= (isp2 ? Hashtbl.find(v, "aoc2021") : 0);
};

let copy = (v, n) => {
  let p = get(v, n, 0);
  let vc = Hashtbl.copy(v);

  Hashtbl.replace(vc, n, p + 1);

  if (p == 1 && (!) @@ is_capitalize(n)) {
    Hashtbl.replace(vc, "aoc2021", 0);
  };

  vc;
};

let get_paths = isp2 => {
  let rec aux = (cur_path, acc, v, curr_n) =>
    List.fold_left(
      (acc', n) =>
        if (n == "end") {
          [List.rev @@ [n, ...cur_path], ...acc'];
        } else if (ican(v, isp2, n)) {
          aux([n, ...cur_path], acc', copy(v, n), n);
        } else {
          acc';
        },
      acc,
    ) @@
    Hashtbl.find(hashtbl, curr_n);

  let v = Hashtbl.create(20);

  Hashtbl.replace(v, "aoc2021") @@ (isp2 ? 1 : 0);

  aux(["start"], [], v, "start");
};

let () = {
  List.iter(extract_node, read_lines("input.txt"));
  List.length @@ get_paths(true) |> print_int;
};
