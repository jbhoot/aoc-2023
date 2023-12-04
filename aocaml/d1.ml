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

let () =
  let lines = "../input/d1.txt" |> Arg.read_arg |> Array.to_list in
  Printf.printf "\nPart 1: %d" (P1.sum lines)
