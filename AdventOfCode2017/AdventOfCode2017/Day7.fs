module Day7
open System.IO
open System.Windows.Forms.VisualStyles.VisualStyleElement.TrackBar
open System.Windows.Forms.ListBox

type Node = 
    {
        name : string;
        weight : int;
        children : string list
    }

let valOrDefault getVal defaultVal key map = 
    if Map.containsKey key map
    then getVal map.[key]
    else defaultVal

let valOrZero = valOrDefault fst 0

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
    |> (List.ofArray>>readName>>readWeight>>readChildren)


let updateMapWithNames map (node:Node) =
    let updateKey map key = 
        let value = valOrZero key map
        Map.add key (value + 1, node) map

    let updated = updateKey map (node.name)
    node.children
    |> List.fold updateKey updated

let parseInput path = 
    path
    |> File.ReadAllLines
    |> Array.map parseRow

let findRoot = Map.pick (fun _ (v,node) -> 
                    if v = 1 
                    then Some(node) 
                    else None)

let calc1 path =
    path
    |> parseInput
    |> Array.fold updateMapWithNames (Map[])
    |> findRoot

let rec foldBackMapTree 
        folder 
        init 
        (map : Map<string, int*Node>)
        rootKey =
    let _,node = map.[rootKey]

    if List.isEmpty node.children
    then init
    else 
        let childResults = 
            List.map 
                (fun c ->foldBackMapTree folder init map c) node.children
        folder childResults node

type Balance = 
   | IsOk of int * int
   | NodeShouldBe of int
let calc2 path =
    let map =     
        path
        |> parseInput
        |> Array.fold updateMapWithNames (Map[])
    
    let root = findRoot map

    let folder childResults Node = 
        if List.

    
    
let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day7-1.txt"

let result1 = calc1 path