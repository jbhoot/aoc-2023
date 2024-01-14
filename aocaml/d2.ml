open Str

type set =
  { red : int
  ; green : int
  ; blue : int
  }

let max_set = { red = 12; green = 13; blue = 14 }

let test_game = "Game 1: 2 blue, 4 green; 7 blue, 1 red, 14 green; 5 blue, 13 green, 1 red; 1 red, 7 blue, 11 green";;

let parse_game_id game =
  let regex = Str.regexp {|^Game \([0-9]+\):|} in
  let _ = Str.search_forward regex game 0 in
  let id_str = Str.matched_group 1 game in
  int_of_string id_str

let extract_game_sets game =
  let regex = Str.regexp {|^Game [0-9]+:\(.*\)$|} in
  let _ = Str.search_forward regex game 0 in
  game |> Str.matched_group 1 |> String.split_on_char ';' |> List.map String.trim
