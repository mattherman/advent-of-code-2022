#load "../Common.fsx"

open Common

let countVisibleTrees =
    Seq.sumBy (fun visible -> if visible then 1 else 0)

let allShorterThan (height: int) =
    Seq.forall (fun otherTreeHeight -> otherTreeHeight < height)

let treeHeight (loc: int * int) (grid: int[][]) =
    let (x, y) = loc
    grid.[y].[x]

let getTreeHeights (grid: int[][]) =
    Seq.map (fun loc -> (treeHeight loc grid))

let isTreeVisible (grid: int[][]) (treeLocation: int * int) =
    let (treeX, treeY) = treeLocation
    let height = treeHeight treeLocation grid
    let north = seq { for y in (treeY - 1) .. -1 .. 0 do (treeX, y) }
    let south = seq { for y in (treeY + 1) .. 1 .. (grid.Length - 1) do (treeX, y) }
    let west = seq { for x in (treeX - 1) .. -1 .. 0 do (x, treeY) }
    let east = seq { for x in (treeX + 1) .. 1 .. (grid.Length - 1) do (x, treeY) }

    List.contains true [
        north |> getTreeHeights grid |> allShorterThan height
        south |> getTreeHeights grid |> allShorterThan height
        west |> getTreeHeights grid |> allShorterThan height
        east |> getTreeHeights grid |> allShorterThan height
    ]

let getTreeVisibility (grid: int[][]) =
    seq {
        for x in 0 .. (grid.Length - 1) do
            for y in 0 .. (grid.Length - 1) do
                isTreeVisible grid (x, y)
    }

let parseGrid (input: seq<string>) =
    input
    |> Seq.map (fun str -> str |> Seq.map (string >> int) |> Seq.toArray)
    |> Seq.toArray

let solve file =
    readInput file
    |> parseGrid
    |> getTreeVisibility
    |> countVisibleTrees

let solution = solve "input.txt"
printfn "%d" solution