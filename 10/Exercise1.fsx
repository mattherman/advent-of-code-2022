#load "../Common.fsx"

open Common

type Instruction =
    | NoOp
    | AddX of int

type Registers = {
    X: int;
}

type Cycle = {
    Start: Registers;
    End: Registers;
}

let parseInstruction line =
    match line with
    | "noop" -> NoOp
    | _ -> match (String.split " " line) with
            | ["addx"; value] -> AddX (int value)
            | _ -> failwith $"Failed to parse instruction: {line}"

let runInstruction instruction registers =
    match instruction with
    | NoOp -> [ { Start = registers; End = registers } ]
    | AddX x -> [ { Start = registers; End = registers }; { Start = registers; End = { registers with X = registers.X + x }} ]

let runProgram instructions =
    let initialState = { Start = { X = 1 }; End = { X = 1 } }
    instructions
    |> Seq.fold (fun cycles instruction ->
        let previousCycle = List.last cycles
        let nextCycles = runInstruction instruction previousCycle.End
        cycles @ nextCycles) [initialState]
    |> Seq.tail

let filterCycles startingCycle period (allCycles: seq<Cycle>) =
    let cycleList = Seq.toList allCycles
    let numberOfCycles = cycleList.Length
    seq {
        for cycleNumber in startingCycle .. period .. numberOfCycles do
            (cycleNumber, cycleList.[cycleNumber - 1])
    }

let signalStrength (cycleNumber, cycle) =
    cycleNumber * cycle.Start.X

let solve file =
    readInput file
    |> Seq.map parseInstruction
    |> runProgram
    |> filterCycles 20 40
    |> Seq.sumBy signalStrength

let args = fsi.CommandLineArgs
if (args.Length > 1) then
    let solution = solve args.[1]
    printfn "%d" solution