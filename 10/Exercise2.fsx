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

let renderDisplay (width: int) (height: int) (cycles: Cycle list) =
    let spritePosition value row =
        let column = value % width
        [(column - 1, row); (column, row); (column + 1, row)]

    let mutable sprite = spritePosition 1 0

    for y in 0 .. (height - 1) do
        sprite <- spritePosition 1 y
        for x in 0 .. (width - 1) do
            let pixel = if List.contains (x, y) sprite then '#' else '.'
            printf "%c" pixel
            let cycleNumber = (x + 1) + (y * width)
            let cycle = cycles.[cycleNumber - 1]
            sprite <- spritePosition cycle.End.X y
        printfn ""

let solve file =
    let cycles =
        readInput file
        |> Seq.map parseInstruction
        |> runProgram
        |> Seq.toList
    renderDisplay 40 6 cycles

let args = fsi.CommandLineArgs
if (args.Length > 1) then
    solve args.[1]