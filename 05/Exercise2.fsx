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
    let operations = Seq.map parseOperation operationLines |> Seq.toList

    (stacks, operations)

let popMultiple (count: int) (stack: Stack<'a>) =
    { 1 .. count }
    |> Seq.toList
    |> List.map (fun i -> stack.Pop())
    |> List.rev
    |> List.toSeq

let pushMultiple (stack: Stack<'a>) (elements: seq<'a>) =
    Seq.iter (fun element -> stack.Push(element)) elements

let moveCrates (source: Stack<Crate>) (destination: Stack<Crate>) count =
    source |> popMultiple count |> pushMultiple destination

let next (stacks: Stack<Crate>[], operations: Operation list) =
    match operations with
    | operation::remaining -> 
        let sourceStack = stacks.[operation.SourceStack - 1]
        let destinationStack = stacks.[operation.DestinationStack - 1]
        moveCrates sourceStack destinationStack operation.CrateCount
        (stacks, remaining)
    | [] -> (stacks, [])

let rec performOperations (stacks: Stack<Crate>[], operations: Operation list) =
    match operations with
    | [] -> stacks
    | _  -> performOperations (next (stacks, operations))

let solve file =
    readInput file
    |> parseInput
    |> performOperations
    |> Array.map (fun stack -> stack.Peek())
    |> Array.map (fun crate -> string crate.[1])
    |> String.concat ""

let solution = solve "input.txt"
printfn "%s" solution