module Day7
open System.IO

type Node = 
    {
        name : string;
        weight : int;
        children : string list
    }

let valOrDefault defaultVal key map = 
    if Map.containsKey key map
    then map.[key]
    else defaultVal

let valOrZero = valOrDefault 0 

let parseRow (row:string) =
    let readName (tok::rest) =
        ({name = tok; children = []; weight = 0}, rest)
    
    let readWeight ((tkn, tokens):Node * string list) =
        let tok::rest = tokens
        let weightStr = tok.Substring(1, (tok.Length - 2))
        {tkn with weight = int weightStr}, rest
    
    let readChildren ((tkn, tokens):Node * string list) = 
        if tokens = []
        then tkn
        else 
            let arrow::rest = tokens
            {tkn with children = rest}
    
    row.Split ([|' ';','|], System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> readName
    |> readWeight
    |> readChildren

let updateMapWithNames map (node:Node) =
    let updateKey map key = 
        let value = valOrZero key map
        Map.add key (value + 1) map

    let updated = updateKey map (node.name)
    node.children
    |> List.fold updateKey updated



let calc path =
    path
    |> File.ReadAllLines
    |> Array.map parseRow
    |> Array.fold updateMapWithNames (Map[])

let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day7-1.txt"

(calc path)
|> Map.filter (fun _ v -> v = 1)
|> printfn "%A"