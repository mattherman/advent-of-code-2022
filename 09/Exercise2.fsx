#load "../Common.fsx"

open Common

type Direction =
    | Up
    | Down
    | Right
    | Left

type State = {
    Knots: list<int * int>;
}

let parseMove line =
    match (String.split " " line) with
    | [direction; distance] ->
        let direction =
            match direction with
            | "U" -> Up
            | "D" -> Down
            | "R" -> Right
            | "L" -> Left
            | _ -> failwith $"Unable to parse line: {line}"
        Seq.replicate (int distance) direction
    | _ -> failwith $"Unable to parse line: {line}"

let parseMoves input =
    Seq.collect parseMove input

let move direction (x, y) =
    match direction with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Right -> (x + 1, y)
    | Left -> (x - 1, y)

let processMove (head: int * int) (tail: int * int) (direction: Direction) =
    let (headX, headY) = head
    let (tailX, tailY) = tail

    let xDiff = headX - tailX
    let xDiffMagnitude = abs xDiff
    let yDiff = headY - tailY
    let yDiffMagnitude = abs yDiff
    let isOrthogonal = xDiff = 0 || yDiff = 0

    if isOrthogonal && (xDiffMagnitude > 1 || yDiffMagnitude > 1) then
        if xDiff > 0 then
            tail |> move Right
        elif xDiff < 0 then
            tail |> move Left
        elif yDiff > 0 then
            tail |> move Up
        else
            tail |> move Down
    elif xDiffMagnitude + yDiffMagnitude > 2 then 
        if xDiff > 0 && yDiff > 0 then
            tail |> move Up |> move Right
        elif xDiff > 0 && yDiff < 0 then
            tail |> move Down |> move Right
        elif xDiff < 0 && yDiff > 0 then
            tail |> move Up |> move Left
        else
            tail |> move Down |> move Left
    else
        tail

let processMoves (state: State) (direction: Direction) =
    let newHead = state.Knots |> List.head |> move direction
    let newKnots =
        state.Knots
        |> List.tail
        |> List.fold (fun previousKnots knot ->
            let head = List.head previousKnots
            let newKnot = processMove head knot direction
            newKnot :: previousKnots) [newHead]
        |> List.rev
    { Knots = newKnots }

let getLogger (debug: bool) =
    if not debug then
        fun (_: State) (_: string) -> ()
    else
        fun (state: State) (header: string) ->
            let allPoints = (0, 0) :: state.Knots
            let minX = List.minBy fst allPoints |> fst
            let maxX = List.maxBy fst allPoints |> fst
            let minY = List.minBy snd allPoints |> snd
            let maxY = List.maxBy snd allPoints |> snd
            let width = (maxX - minX) + 1
            let height = (maxY - minY) + 1
            let grid = Array2D.zeroCreate<string> height width

            let adjustWindow (x, y) =
                (x - minX, y - minY)
            let adjustOrigin (x: int, y: int) =
                (x, height - 1 - y)
            let transform = (adjustWindow >> adjustOrigin)

            // s: (0, 0), 1: (3, 5), 2: (-1, -2)
            // minX = -1, maxX = 3, width = 5
            // minY = -2, maxY = 5, height = 8
            //
            // (0, 0)   => (x - minX, y - minY) => (1, 2)
            // (3, 5)   => (x - minX, y - minY) => (4, 7)
            // (-1, -2) => (x - minX, y - minY) => (0, 0)
            //
            // (1, 2)   => (x, height - 1 - y)  => (1, 3)
            // (4, 7)   => (x, height - 1 - y)  => (4, 0)
            // (0, 0)   => (x, height - 1 - y)  => (0, 7)
            // ....1
            // .....
            // .....
            // .....
            // .s...
            // .....
            // .....
            // 2....

            let (originX, originY) = transform (0, 0)
            Array2D.set grid originY originX "S"
            let (headX, headY) = transform (List.head state.Knots)
            Array2D.set grid headY headX "H"

            state.Knots
            |> List.tail
            |> List.iteri (fun index knot ->
                let (x, y) = transform knot
                let currentLabel = Array2D.get grid y x
                if currentLabel = null || currentLabel = "S" then
                    Array2D.set grid y x (string (index + 1)))

            printfn "%s" header
            for y in 0 .. (height - 1) do
                printf "\t"
                for x in 0 .. (width - 1) do
                    let value = Array2D.get grid y x
                    match value with
                    | null | "" -> printf ". "
                    | _ ->  printf "%s " value
                printfn ""
            printfn $"({minX}, {minY})"

let solve file debug =
    let print = getLogger debug
    let moves = readInput file |> parseMoves
    let initialState = { Knots = (List.replicate 10 (0, 0))}
    print initialState "== Initial State =="
    let states =
        moves
        |> Seq.fold (fun states nextMove ->
            let currentState = List.head states
            let nextState = processMoves currentState nextMove
            print nextState $"\n== State {List.length states} [Move: {nextMove}] =="
            nextState :: states) [initialState]
    states
    |> List.map (fun state -> List.last state.Knots)
    |> List.distinct
    |> List.length

let args = fsi.CommandLineArgs
if (args.Length > 1) then
    let file = args.[1]
    let debug = args.Length > 2 && args.[2] = "debug"
    let solution = solve file debug
    printfn "%d" solution