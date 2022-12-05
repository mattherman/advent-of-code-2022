#load "../Common.fsx"

open Common
open System

let parseRucksack = Set.ofSeq

let findBadge elfGroup =
    Seq.reduce (Set.intersect) elfGroup |> Seq.head

let itemPriority (itemChar: char) =
    if Char.IsUpper(itemChar) then
        (int itemChar) - 38
    else
        (int itemChar) - 96

let solve file =
    readInput file
    |> Seq.map parseRucksack
    |> Seq.chunkBySize 3
    |> Seq.map (findBadge >> itemPriority)
    |> Seq.sum

let solution = solve "input.txt"
printfn "%d" solution