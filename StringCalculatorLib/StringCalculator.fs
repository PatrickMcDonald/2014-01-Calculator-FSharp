module StringCalculator

open System

let Add (arg:string) =
    let (|CustomDelimiter|) (s:string) =
        if s.StartsWith("//") then
            ([| s.Substring(2, s.IndexOf("\n")-2) |], s.Substring(s.IndexOf("\n")))
        else
            ([| ","; "\n" |], s)

    let parse s =
        match s with
        | "" -> raise (ArgumentException("Missing number"))
        | _ ->
            match Int32.Parse(s) with
            | x when x < 0 -> raise (ArgumentException(sprintf "negatives not allowed: %d" x))
            | x when x > 1000 -> 0
            | x -> x

    let args =
        match arg with
        | "" -> []
        | CustomDelimiter (delimiter,text) -> 
            text.Split(delimiter, StringSplitOptions.None)
            |> Seq.map parse
            |> List.ofSeq

    let rec AddRec sum list =
        match list with
        | [] -> sum
        | head::tail -> AddRec (sum + head) tail // tail recursive

    AddRec 0 args

