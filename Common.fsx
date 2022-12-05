let readInput filename =
    System.IO.File.ReadAllLines(filename) |> Array.toSeq

module String =
    let split (separator: string) (str: string) =
        str.Split separator
        |> Array.toList