#load "../Common.fsx"

open Common
open System.Text.RegularExpressions
open System.Collections.Generic

type Operation = {
    SourceStack: int;
    DestinationStack: int;
    CrateCount: int;
}

type Crate = string

let (|Operation|_|) str =
    let group (index: int) (m: Match) =
        (m.Groups.Item index).Value

    let m = Regex.Match(str, "move (\\d+) from (\\d+) to (\\d+)")

    if (m.Success && m.Groups.Count = 4) then
        Some { 
            CrateCount = int (m |> group 1);
            SourceStack = int (m |> group 2);
            DestinationStack = int (m |> group 3)
        }
    else
        None

let parseCrates (stackLine: string) : Crate seq =
    stackLine
    // "[A] [B]     [C]"
    |> Seq.chunkBySize 4 
    // [ ['['; 'A'; ']'; ' ']; ['['; 'B'; ']'; ' ']; [' '; ' '; ' '; ' ']; ['['; 'C'; ']'; ' '] ]
    |> Seq.map System.String 
    // ["[A] "; "[B] "; "    "; "[C] "]
    |> Seq.map (fun s -> s.Trim())
    // ["[A]"; "[B]"; ""; "[C]"]

let parseStacks lines =
    let linesReversed = Seq.rev lines
    let infoLine = Seq.head linesReversed
    let count = 
        infoLine
        |> String.split " "
        |> Seq.filter (fun x -> not (x = ""))
        |> Seq.length
    let stacks =
        Array.zeroCreate count
        |> Array.map (fun _ -> new Stack<Crate>())

    let stackLines = linesReversed |> Seq.skip 1

    stackLines |> Seq.iter (fun stackLine ->
        stackLine
        |> parseCrates
        |> Seq.iteri (fun index crate ->
            if (not (String.isNullOrWhitespace crate)) then
                stacks.[index].Push(crate)))

    stacks

let parseOperation line =
    match line with
    | Operation operation -> operation
    | _ -> failwith $"Failed to parse operation: {line}"

let parseInput input =
    let stackLines = Seq.takeWhile (fun line -> not (line = "")) input
    let operationLines = Seq.skip ((Seq.length stackLines) + 1) input

    let stacks = parseStacks stackLines
    let operations = Seq.map parseOperation operationLines

    (stacks, operations)

let solve file =
    let input = readInput file
    let (stacks, operations) = parseInput input
    0

let solution = solve "test_input.txt"
printfn "%d" solution