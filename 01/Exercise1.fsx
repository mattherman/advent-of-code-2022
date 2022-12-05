#load "../Common.fsx"

open Common

// [
//     "1"
//     "2"
//     ""
//     "3"
//     "4"
//     "5"
//     ""
//     "6"
// ] -> [[1; 2]; [3; 4; 5] [6]]

let splitMap predicate mapper list=
    seq {
        let mutable nextChunk = []
        for item in list do
            if (predicate item) then
                yield nextChunk
                nextChunk <- []
            else
                nextChunk <- (mapper item) :: nextChunk
    }

let solve file =
    let input = readInput file
    let elves =
        input 
        |> splitMap (fun s -> s = "") (fun s -> (int s))
    let highestCalorieFood =
        elves
        |> Seq.maxBy (List.sum)
    List.sum highestCalorieFood

let solution = solve "input.txt"
printfn "%d" solution