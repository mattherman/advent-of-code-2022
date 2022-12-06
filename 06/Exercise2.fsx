#load "../Common.fsx"

open Common

let allElementsUnique (arr: 'T[]) =
    let distinctElements = Array.distinct arr
    distinctElements.Length = arr.Length

let findStartOfPacketMarker signal =
    let startOfPacketMarker =
        signal
        |> Seq.windowed 14
        |> Seq.find allElementsUnique
        |> System.String
    match (String.split startOfPacketMarker signal) with
    | [ garbage; _ ] -> garbage.Length + startOfPacketMarker.Length
    | _ -> failwith $"Failed to parse signal | Start-of-packet marker = {startOfPacketMarker}"

let solve file =
    readInput file
    |> Seq.head
    |> findStartOfPacketMarker

let solution = solve "input.txt"
printfn "%d" solution