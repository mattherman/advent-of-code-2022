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

let isEitherContainedIn (first, second) =
    Set.isSubset first second || Set.isSubset second first

let solve file =
    readInput file
    |> Seq.map parsePair
    |> Seq.filter isEitherContainedIn
    |> Seq.length

let solution = solve "input.txt"
printfn "%d" solution