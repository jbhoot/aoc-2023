open System

let ParseLine line =
    let onlyDigits = line |> String.filter (Char.IsDigit)
    let firstLast = onlyDigits[0].ToString() + (Seq.last onlyDigits).ToString()
    Int32.Parse firstLast
    
let Main () =
    "../input/1.txt"
    |> IO.File.ReadLines
    |> Seq.map ParseLine
    |> Seq.reduce (fun x y -> x + y)
    |> Console.WriteLine
    
Main()