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

type Display = char[][]

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

let initializeDisplay (width: int) (height: int) : Display =
    Array.init height (fun _ ->
        Array.init width (fun _ -> '.'))

let renderDisplay (width: int) (height: int) (cycles: Cycle list) =
    let display = initializeDisplay 40 6
    let mutable sprite = [(0,0); (1,0); (2,0)]

    // TODO: There's no vertical position - the sprite just moves left/right and is applied to each row in turn
    let spritePosition value row =
        let column = value % width
        [(column - 1, row); (column, row); (column + 1, row)]

    for y in 0 .. (height - 1) do
        for x in 0 .. (width - 1) do
            let cycleNumber = (x + 1) + (y * width)
            printfn "%d: %A -> %A" cycleNumber sprite (x, y)
            display.[y].[x] <-
                if List.contains (x, y) sprite then '#' else '.'
            let cycle = cycles.[cycleNumber - 1]
            sprite <- spritePosition cycle.End.X y

    for y in 0 .. (height - 1) do
        for x in 0 .. (width - 1) do
            printf "%c" display.[y].[x]
        printfn ""

let solve file =
    let cycles =
        readInput file
        |> Seq.map parseInstruction
        |> runProgram
        |> Seq.toList
    renderDisplay 40 6 cycles
    0

let args = fsi.CommandLineArgs
if (args.Length > 1) then
    let solution = solve args.[1]
    printfn "%d" solution