#load "../Common.fsx"

open Common

type Direction =
    | Up
    | Down
    | Right
    | Left

type State = {
    Head: int * int;
    Tail: int * int;
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

let processMove (state: State) (direction: Direction) =
    let newHead = state.Head |> move direction
    let (headX, headY) = newHead
    let (tailX, tailY) = state.Tail;

    let xDiff = headX - tailX
    let xDiffMagnitude = abs xDiff
    let yDiff = headY - tailY
    let yDiffMagnitude = abs yDiff
    let isOrthogonal = xDiff = 0 || yDiff = 0

    if isOrthogonal && (xDiffMagnitude > 1 || yDiffMagnitude > 1) then
        let newTail = state.Tail |> move direction
        { Head = newHead; Tail = newTail }
    elif xDiffMagnitude + yDiffMagnitude > 2 then 
        let newTail =
            if xDiff > 0 && yDiff > 0 then
                state.Tail |> move Up |> move Right
            elif xDiff > 0 && yDiff < 0 then
                state.Tail |> move Down |> move Right
            elif xDiff < 0 && yDiff > 0 then
                state.Tail |> move Up |> move Left
            else
                state.Tail |> move Down |> move Left
        { Head = newHead; Tail = newTail }
    else
        { Head = newHead; Tail = state.Tail }

let solve file =
    let moves = readInput file |> parseMoves
    let initialState = { Head = (0, 0); Tail = (0, 0)}
    let states =
        moves
        |> Seq.fold (fun states nextMove ->
            let currentState = List.head states
            let nextState = processMove currentState nextMove
            nextState :: states) [initialState]
    states
    |> List.map (fun state -> state.Tail)
    |> List.distinct
    |> List.length

let solution = solve "input.txt"
printfn "%d" solution