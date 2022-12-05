#load "../Common.fsx"

open Common

type Choice =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Win
    | Lose
    | Draw

let score choice =
    match choice with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let parseChoice choice =
    match choice with
    | "A" -> Rock
    | "B" -> Paper
    | _   -> Scissors

let parseOutcome outcome =
    match outcome with
    | "X" -> Lose
    | "Y" -> Draw
    | _   -> Win

let parseGame game =
    match (String.split " " game) with
    | [opponent; outcome] -> (opponent |> parseChoice, outcome |> parseOutcome)
    | _ -> failwith $"Unable to parse game: {game}"

let choiceForOutcome (opponentChoice, desiredOutcome) =
    match (opponentChoice, desiredOutcome) with
    | (Rock, Draw) | (Paper, Lose) | (Scissors, Win)  -> (Rock, desiredOutcome)
    | (Rock, Win)  | (Paper, Draw) | (Scissors, Lose) -> (Paper, desiredOutcome)
    | _                                               -> (Scissors, desiredOutcome)

let scoreGame (playerChoice, outcome) =
    let choiceScore = score playerChoice
    match outcome with
    | Win -> 6 + choiceScore
    | Draw -> 3 + choiceScore
    | Lose -> choiceScore

let solve file =
    let input = readInput file
    let games = Seq.map parseGame input
    games |> Seq.map choiceForOutcome |> Seq.map scoreGame |> Seq.sum

let solution = solve "input.txt"
printfn "%d" solution