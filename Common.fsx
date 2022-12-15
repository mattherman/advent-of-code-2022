let readInput filename =
    System.IO.File.ReadAllLines(filename) |> Array.toSeq

module String =
    let split (separator: string) (str: string) =
        str.Split separator
        |> Array.toList

    let trimAndSplit (separator: string) (str: string) =
        split separator (str.Trim())

    let isNullOrWhitespace (str: string) =
        System.String.IsNullOrWhiteSpace(str)

module Seq =
    let split (separator: 'a) (sequence: seq<'a>) : seq<'a list> =
        seq {
            let mutable nextChunk = []
            for item in sequence do
                if item = separator then
                    yield (List.rev nextChunk)
                    nextChunk <- []
                else
                    nextChunk <- item :: nextChunk
        }