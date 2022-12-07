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
    Children: FileSystemItem list
    Parent: Directory option;
}

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

let getName (item: FileSystemItem) =
    match item with
    | Directory d -> d.Name
    | File f -> f.Name

let changeDirectory (arguments: string[]) (currentDirectory: Directory) =
    match arguments with
    | [|".."|] -> 
        match currentDirectory.Parent with
        | Some p -> p
        | None -> failwith $"Could not switch to parent of directory: {currentDirectory.Name}"
    | [|dir|] -> 
        match (findInDirectory currentDirectory dir) with
        | Some (Directory d) -> d
        | None -> { Name = dir; Parent = Some currentDirectory; Children = []}
        | _ -> failwith $"Supplied filename to 'cd' command: {dir}"
    | _ -> 
        let args = String.concat " " arguments
        failwith $"Incorrect number of arguments for 'cd' command: {args}"

let parseTerminalOutput (output: seq<string>) =
    let mutable currentDirectory = { Name = "/"; Parent = None; Children = [] }
    for line in output do
        if line.StartsWith('$') then
            printfn $"{line}"
            let input = parseUserInput line
            currentDirectory <-
                match input.Command with
                | "cd" -> changeDirectory input.Arguments currentDirectory
                | "ls" -> currentDirectory
                | _ -> failwith $"Invalid command: {input.Command}"
            printfn $"-> {currentDirectory.Name}"
        else
            let directoryItem = parseOutput line currentDirectory
            currentDirectory <- { currentDirectory with Children = directoryItem :: currentDirectory.Children }
            printfn $"-> {currentDirectory.Name} + {getName directoryItem}"

let solve file =
    readInput file |> parseTerminalOutput
    0

let solution = solve "test_input.txt"
printfn "%d" solution