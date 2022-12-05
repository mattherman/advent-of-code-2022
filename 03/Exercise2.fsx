#load "../Common.fsx"

open Common
open System

let parseRucksack (rucksack: string) =
    Set.ofSeq rucksack

let findBadge elfGroup =
    Seq.reduce (Set.intersect) elfGroup |> Seq.head

let itemPriority (itemChar: char) =
    if Char.IsUpper(itemChar) then
        (int itemChar) - 38
    else
        (int itemChar) - 96

let solve file =
    let input = readInput file
    let rucksacks = Seq.map parseRucksack input
    let elfGroups = Seq.chunkBySize 3 rucksacks
    elfGroups |> Seq.map (findBadge >> itemPriority) |> Seq.sum

let solution = solve "input.txt"
printfn "%d" solution