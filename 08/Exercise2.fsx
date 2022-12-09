#load "../Common.fsx"

open Common

let treeHeight (loc: int * int) (grid: int[][]) =
    let (x, y) = loc
    grid.[y].[x]

let getVisibleTrees (grid: int[][]) (height: int) (trees: seq<int * int>) =
    let mutable blocked = false
    seq {
        for otherTree in trees do
            if not blocked then
                if (treeHeight otherTree grid) >= height then
                    blocked <- true
                yield otherTree
    }

let calculateScenicScore (grid: int[][]) (treeLocation: int * int) =
    let (treeX, treeY) = treeLocation
    let height = treeHeight treeLocation grid
    let north = seq { for y in (treeY - 1) .. -1 .. 0 do (treeX, y) }
    let south = seq { for y in (treeY + 1) .. 1 .. (grid.Length - 1) do (treeX, y) }
    let west = seq { for x in (treeX - 1) .. -1 .. 0 do (x, treeY) }
    let east = seq { for x in (treeX + 1) .. 1 .. (grid.Length - 1) do (x, treeY) }

    List.fold (*) 1 [
        north |> getVisibleTrees grid height |> Seq.length
        south |> getVisibleTrees grid height |> Seq.length
        west |> getVisibleTrees grid height |> Seq.length
        east |> getVisibleTrees grid height |> Seq.length
    ]

let getTreeScores (grid: int[][]) =
    seq {
        for x in 0 .. (grid.Length - 1) do
            for y in 0 .. (grid.Length - 1) do
                let tree = (x, y)
                (tree, calculateScenicScore grid tree)
    }

let parseGrid (input: seq<string>) =
    input
    |> Seq.map (fun str -> str |> Seq.map (string >> int) |> Seq.toArray)
    |> Seq.toArray

let solve file =
    let idealLocation =
        readInput file
        |> parseGrid
        |> getTreeScores
        |> Seq.maxBy snd
    idealLocation |> snd 

let solution = solve "input.txt"
printfn "%d" solution