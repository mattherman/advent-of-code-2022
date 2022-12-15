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

type Monkey = {
    Items: int list;
    Operation: BinaryOperation;
}

type State = {
    Monkeys: Monkey list;
}

let group (index: int) (m: Match) =
        (m.Groups.Item index).Value

let parseItems itemsLine =
    match (String.trimAndSplit ": " itemsLine) with
    | ["Starting items"; csv] ->
        let items = String.trimAndSplit ", " csv
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
    match (String.trimAndSplit ": " operationLine) with
    | ["Operation"; op] ->
        let m = Regex.Match(op, @"new = ([a-zA-Z0-9]+) ([\+\*]) ([a-zA-Z0-9]+)")
        if (m.Success && m.Groups.Count = 4) then
            let left = m |> group 1 |> parseValue
            let operator = m |> group 2 |> parseOperator
            let right = m |> group 3 |> parseValue
            { Left = left; Operator = operator; Right = right }
        else
            failwith $"Unable to parse operation: {op}"
    | _ -> failwith $"Unable to parse operation: {operationLine}"

let parseMonkey lines =
    match lines with
    | [_; itemsLine; operationLine; _; _; _] ->
        let items = parseItems itemsLine
        let operation = parseOperation operationLine
        { Items = items; Operation = operation }
    | _ -> failwith $"""Unable to parse monkey: {String.concat "\n" lines}"""

let parse input =
    input
    |> Seq.split ""
    |> Seq.map parseMonkey

let solve file =
    let monkeys = readInput file |> parse |> Seq.toList
    let initialState = { Monkeys = monkeys }
    0

let args = fsi.CommandLineArgs
if (args.Length > 1) then
    let solution = solve args.[1]
    printfn "%d" solution