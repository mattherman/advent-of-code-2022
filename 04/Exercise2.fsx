#load "../Common.fsx"

open Common

let rangeSet range =
    match (String.split "-" range) with
    | [start; finish] -> seq { (int start) .. (int finish) } |> Set.ofSeq
    | _ -> failwith $"Failed to parse range: {range}"

let parsePair line =
    match (String.split "," line) with
    | [left; right] -> (left |> rangeSet, right |> rangeSet)
    | _ -> failwith $"Failed to parse pair: {line}"

let anyOverlap (first, second) =
    let overlaps a b =
        Seq.length (Set.intersect a b) > 0
    overlaps first second || overlaps second first

let solve file =
    readInput file
    |> Seq.map parsePair
    |> Seq.filter anyOverlap
    |> Seq.length

let solution = solve "input.txt"
printfn "%d" solution