open System

let Tokens =
    [ "1"
      "2"
      "3"
      "4"
      "5"
      "6"
      "7"
      "8"
      "9"
      "0"
      "one"
      "two"
      "three"
      "four"
      "five"
      "six"
      "seven"
      "eight"
      "nine"
      "zero" ]

let ToDigit token =
    match token with
    | "one"
    | "1" -> "1"
    | "two"
    | "2" -> "2"
    | "three"
    | "3" -> "3"
    | "four"
    | "4" -> "4"
    | "five"
    | "5" -> "5"
    | "six"
    | "6" -> "6"
    | "seven"
    | "7" -> "7"
    | "eight"
    | "8" -> "8"
    | "nine"
    | "9" -> "9"
    | "zero"
    | "0" -> "0"
    | _ -> failwith $"Invalid string {token}"

let ParseLine (line: string) =
    let (first, _) =
        Tokens
        |> List.map (fun t -> (t, line.IndexOf(t)))
        |> List.filter (fun (_, i) -> i >= 0)
        |> List.minBy (snd)

    let (last, _) =
        Tokens
        |> List.map (fun t -> (t, line.LastIndexOf(t)))
        |> List.filter (fun (_, i) -> i >= 0)
        |> List.maxBy (snd)

    let firstLast = (ToDigit first) + (ToDigit last)
    
    Int32.Parse firstLast
    
let Main () =
    "../input/1.txt"
    |> IO.File.ReadLines
    |> Seq.map ParseLine
    |> Seq.reduce (fun x y -> x + y)
    |> Console.WriteLine
    
Main()
