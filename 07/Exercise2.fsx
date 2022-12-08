#load "../Common.fsx"

open Common

type Input = {
    Command: string;
    Arguments: string[];
}

type FileSystemItem =
    | File of File
    | Directory of Directory
and File = {
    Name: string;
    Size: int;
}
and Directory = {
    Name: string;
    mutable Children: FileSystemItem list;
    Parent: Directory option;
}

let getName (item: FileSystemItem) =
    match item with
    | Directory d -> d.Name
    | File f -> f.Name

let sortFileSystemItems (items: FileSystemItem list) =
    items
    |> List.sortBy getName
    |> List.sortBy (fun item ->
        match item with
        | File f -> -1
        | Directory d -> 1)

let rec printFileSystemItem (depth: int) (item: FileSystemItem) =
    let indent = String.replicate depth "  "
    match item with
    | File f ->
        printfn $"{indent}{f.Size} {f.Name}"
    | Directory d ->
        printfn $"{indent}/{d.Name}"
        List.iter (printFileSystemItem (depth + 1)) (sortFileSystemItems d.Children)

let printFileTree (rootDirectory: Directory) =
    printfn $"/"
    List.iter (printFileSystemItem 1) (sortFileSystemItems rootDirectory.Children)

let parseUserInput line =
    match (String.split " " line) with
    | marker :: command :: arguments ->
        { Command = command; Arguments = Seq.toArray arguments }
    | _ -> failwith $"Failed to parse input: {line}"

let parseOutput (line: string) (parentDirectory: Directory) =
    match (String.split " " line) with
    | [ "dir"; directoryName ] ->
        Directory { Name = directoryName; Parent = Some parentDirectory; Children = [] }
    | [ size; filename ] ->
        File { Name = filename; Size = int size }
    | _ -> failwith $"Failed to parse output: {line}"

let findInDirectory (directory: Directory) (name: string) =
    directory.Children
    |> List.tryFind (fun fileSystemItem ->
        match fileSystemItem with
        | File f -> f.Name = name
        | Directory d -> d.Name = name)

let rec findRootDirectory (directory: Directory) =
    match directory.Parent with
    | Some d -> findRootDirectory d
    | None -> directory

let rec findSubDirectories (directory: Directory) =
    directory.Children
    |> List.collect (fun item ->
        match item with
        | Directory d -> d :: findSubDirectories d
        | _ -> [])

let rec calculateDirectorySize (directory: Directory) =
    directory.Children
    |> List.sumBy (fun item ->
        match item with
        | File f -> f.Size
        | Directory d -> calculateDirectorySize d)

let changeDirectory (arguments: string[]) (currentDirectory: Directory) =
    match arguments with
    | [|"/"|] ->
        findRootDirectory currentDirectory
    | [|".."|] -> 
        match currentDirectory.Parent with
        | Some p -> p
        | None -> failwith $"Could not switch to parent of directory: {currentDirectory.Name}"
    | [|dir|] ->
        match (findInDirectory currentDirectory dir) with
        | Some (Directory d) -> d
        | None ->
            let newDirectory = { Name = dir; Parent = Some currentDirectory; Children = [] }
            currentDirectory.Children <- (Directory newDirectory)::currentDirectory.Children
            newDirectory
        | _ -> failwith $"Supplied filename to 'cd' command: {dir}"
    | _ -> 
        let args = String.concat " " arguments
        failwith $"Incorrect number of arguments for 'cd' command: {args}"

let parseTerminalOutput (output: seq<string>) =
    let mutable currentDirectory = { Name = "/"; Parent = None; Children = [] }
    for line in output do
        if line.StartsWith('$') then
            let input = parseUserInput line
            currentDirectory <-
                match input.Command with
                | "cd" ->
                    changeDirectory input.Arguments currentDirectory
                | "ls" -> currentDirectory
                | _ -> failwith $"Invalid command: {input.Command}"
        else
            let directoryItem = parseOutput line currentDirectory
            currentDirectory.Children <- directoryItem :: currentDirectory.Children
    findRootDirectory currentDirectory

let solve file =
    let rootDirectory = readInput file |> parseTerminalOutput
    printFileTree rootDirectory

    let totalUsedDiskSpace = calculateDirectorySize rootDirectory
    printfn "Total used disk space = %d" totalUsedDiskSpace
    let remainingDiskSpace = 70_000_000 - totalUsedDiskSpace
    printfn "Remaining disk space = %d" remainingDiskSpace
    let diskSpaceToFree = 30_000_000 - remainingDiskSpace
    printfn "Disk space to free = %d" diskSpaceToFree

    let directoriesOfSufficientSize =
        rootDirectory
        |> findSubDirectories
        |> List.map (fun d -> (d.Name, calculateDirectorySize d))
        |> List.filter (fun (_, size) -> size > diskSpaceToFree)
    directoriesOfSufficientSize
    |> List.minBy snd
    |> snd

let solution = solve "input.txt"
printfn "%d" solution