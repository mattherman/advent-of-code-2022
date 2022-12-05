#load "../Common.fsx"

open Common

type Choice =
    | Rock
    | Paper
    | Scissors

let score choice =
    match choice with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let parseChoice choice =
    match choice with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | _         -> Scissors

let parseGame game =
    match (String.split " " game) with
    | [opponent; player] -> (opponent |> parseChoice, player |> parseChoice)
    | _ -> failwith $"Unable to parse game: {game}"

let scoreGame (opponentChoice, playerChoice) =
    let gamePoints =
        match (playerChoice, opponentChoice) with
        | (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) -> 6
        | (_, _) when playerChoice = opponentChoice -> 3
        | _ -> 0
    gamePoints + (score playerChoice)

let solve file =
    let input = readInput file
    let games = Seq.map parseGame input
    games |> Seq.map scoreGame |> Seq.sum

let solution = solve "input.txt"
printfn "%d" solution