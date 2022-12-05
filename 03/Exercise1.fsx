#load "../Common.fsx"

open Common
open System

let parseRucksack rucksack =
    let compartments = Seq.splitInto 2 rucksack |> Seq.toList
    match compartments with
    | [first; second] -> (Set.ofSeq first, Set.ofSeq second)
    | _ -> failwith $"Failed to parse rucksack: {rucksack}"

let findMistake (firstCompartment, secondCompartment) =
    Set.intersect firstCompartment secondCompartment
    |> Set.toList
    |> List.head

let itemPriority (itemChar: char) =
    if Char.IsUpper(itemChar) then
        (int itemChar) - 38
    else
        (int itemChar) - 96

let solve file =
    readInput file
    |> Seq.map (parseRucksack >> findMistake >> itemPriority)
    |> Seq.sum

let solution = solve "input.txt"
printfn "%d" solution