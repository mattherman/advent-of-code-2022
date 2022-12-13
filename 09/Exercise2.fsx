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
        tail |> move direction
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

let print (state: State) (header: string) =
    let allPoints = (0, 0) :: state.Knots
    let minX = List.minBy fst allPoints |> fst
    let maxX = List.maxBy fst allPoints |> fst
    let minY = List.minBy snd allPoints |> snd
    let maxY = List.maxBy snd allPoints |> snd
    let width = (maxX - minX) + 1
    let height = (maxY - minY) + 1
    let mutable grid = Array2D.zeroCreate<string> width height

    let transform (x, y) =
        (x - minX, y - minY)

    // (0, 0) (3, 5) (-1, -2)
    // minX = -1, maxX = 3, width = 5
    // minY = -2, maxY = 5, height = 8
    //
    // (0, 0)   => (x - minX, y - minY) => (1, 2)
    // (3, 5)   => (x - minX, y - minY) => (4, 7)
    // (-1, -2) => (x - minX, y - minY) => (0, 0)
    // 00001
    // 00000
    // 00000
    // 00000
    // 00000
    // 0s000
    // 00000
    // 20000

    let (originX, originY) = transform (0, 0)
    Array2D.set grid originX originY "S"
    let (headX, headY) = transform (List.head state.Knots)
    Array2D.set grid headX headY "H"

    state.Knots
    |> List.tail
    |> List.iteri (fun index knot ->
        let (x, y) = transform knot
        Array2D.set grid x y (string (index + 1)))

    printfn "%s" header
    for y in (height - 1) .. -1 .. 0 do
        for x in (width - 1) .. -1 .. 0 do
            let value = Array2D.get grid x y
            match value with
            | "" -> printf "."
            | _ ->  printf "%s" value
        printfn ""

let solve file =
    let moves = readInput file |> parseMoves
    let initialState = { Knots = (List.replicate 10 (0, 0))}
    let states =
        moves
        |> Seq.fold (fun states nextMove ->
            let currentState = List.head states
            let nextState = processMoves currentState nextMove
            print nextState $"== State {List.length states} =="
            nextState :: states) [initialState]
    states
    |> List.map (fun state -> List.last state.Knots)
    |> List.distinct
    |> List.length

let solution = solve "test_input.txt"
printfn "%d" solution