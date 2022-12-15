#load "../Common.fsx"

open Common
open System.Text.RegularExpressions

type Value =
    | Old
    | Literal of int

type BinaryOperation = {
    Left: Value;
    Operator: int -> int -> int
    Right: Value;
}

let group (index: int) (m: Match) =
        (m.Groups.Item index).Value

let parseItems itemsLine =
    match (String.split ": " itemsLine) with
    | ["Staring items"; csv] ->
        let items = String.split ", " csv
        List.map int items
    | _ -> failwith $"Unable to parse items: {itemsLine}"

let parseValue str =
    match str with
    | "old" -> Old
    | _ ->
        match System.Int32.TryParse str with
        | (true, number) -> 
            Literal (int number)
        | _ -> failwith $"Failed to parse value: {str}"

let parseOperator str =
    match str with
    | "+" -> (fun a b -> a + b)
    | "*" -> (fun a b -> a * b)
    | _ -> failwith $"Failed to parse operator: {str}"

let parseOperation operationLine =
    match (String.split ": " operationLine) with
    | ["Operation"; op] ->
        let m = Regex.Match(@"new = ([a-zA-Z0-9]+) ([\+\*]) ([a-zA-Z0-9]+)", op)
        if (m.Success && m.Groups.Count = 4) then
            let left = m |> group 1 |> parseValue
            let operator = m |> group 2 |> parseOperator
            let right = m |> group 3 |> parseValue
            { Left = left; Operator = operator; Right = right }
        else
            failwith $"Unable to parse operation: {op}"
    | _ -> failwith $"Unable to parse operation: {operationLine}"

let solve file =
    0

let args = fsi.CommandLineArgs
if (args.Length > 1) then
    let solution = solve args.[1]
    printfn "%d" solution