#load "../Common.fsx"

open Common

let solve file =
    0

let args = fsi.CommandLineArgs
if (args.Length > 1) then
    let solution = solve args.[1]
    printfn "%d" solution