module P1 = struct
  let is_digit x =
    match x with
    | '0' .. '9' -> true
    | _ -> false

  let parse_line line =
    let only_digits =
      line |> String.to_seq |> List.of_seq |> List.filter is_digit
    in
    let first = only_digits |> List.hd in
    let last = only_digits |> List.rev |> List.hd in
    let first_last = [ first; last ] |> List.to_seq |> String.of_seq in
    int_of_string first_last

  let sum lines =
    lines |> List.map parse_line |> List.fold_left (fun acc x -> acc + x) 0
end

module P2 = struct
  let tokens =
    [ "1"
    ; "2"
    ; "3"
    ; "4"
    ; "5"
    ; "6"
    ; "7"
    ; "8"
    ; "9"
    ; "0"
    ; "one"
    ; "two"
    ; "three"
    ; "four"
    ; "five"
    ; "six"
    ; "seven"
    ; "eight"
    ; "nine"
    ; "zero"
    ]

  let to_digit token =
    match token with
    | "one"
    | "1" ->
      "1"
    | "two"
    | "2" ->
      "2"
    | "three"
    | "3" ->
      "3"
    | "four"
    | "4" ->
      "4"
    | "five"
    | "5" ->
      "5"
    | "six"
    | "6" ->
      "6"
    | "seven"
    | "7" ->
      "7"
    | "eight"
    | "8" ->
      "8"
    | "nine"
    | "9" ->
      "9"
    | "zero"
    | "0" ->
      "0"
    | _ -> failwith (Printf.sprintf "Invalid string %s" token)

  let min_by accessor_fn lst =
    List.fold_left
      (fun acc curr ->
        let acc_v = accessor_fn acc in
        let curr_v = accessor_fn curr in
        if curr_v < acc_v then
          curr
        else
          acc)
      (lst |> List.hd) lst

  let max_by accessor_fn lst =
    List.fold_left
      (fun acc curr ->
        let acc_v = accessor_fn acc in
        let curr_v = accessor_fn curr in
        if curr_v > acc_v then
          curr
        else
          acc)
      (lst |> List.hd) lst

  let index_of_substr substr str =
    let rgx = Str.regexp_string substr in
    match Str.search_forward rgx str 0 with
    | v -> v
    | exception Not_found -> -1

  let last_index_of_substr substr str =
    let rgx = Str.regexp_string substr in
    match Str.search_backward rgx str (String.length str) with
    | v -> v
    | exception Not_found -> -1

  let parse_line line =
    let first, _ =
      tokens
      |> List.map (fun t -> (t, index_of_substr t line))
      |> List.filter (fun (_, i) -> i >= 0)
      |> min_by snd
    in
    let last, _ =
      tokens
      |> List.map (fun t -> (t, last_index_of_substr t line))
      |> List.filter (fun (_, i) -> i >= 0)
      |> max_by snd
    in
    let first_last = to_digit first ^ to_digit last in
    int_of_string first_last

  let sum lines =
    lines |> List.map parse_line |> List.fold_left (fun acc x -> acc + x) 0
end

let () =
  let lines = "../input/d1.txt" |> Arg.read_arg |> Array.to_list in
  Printf.printf "\nPart 1: %d" (P1.sum lines);
  Printf.printf "\nPart 2: %d\n" (P2.sum lines)
