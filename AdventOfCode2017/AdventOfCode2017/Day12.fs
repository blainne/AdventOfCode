module Day12
open System

let parseRow (row:string) =
    let tokens = row.Split([|" "; ","|],StringSplitOptions.RemoveEmptyEntries)
    let pid = int tokens.[0]
    let pids = tokens.[2..] |> Array.map int
    (pid, pids)

let keyValueOrDefault key ``default`` map =
    if Map.containsKey key map
    then map.[key]
    else ``default``

let updateEntry k v map =
        let current = keyValueOrDefault k [] map
        Map.add k (v::current) map

let updateMapping map (pid, pids) =
    pids
    |> Array.fold 
        (fun m p -> updateEntry pid p m)
        map

let parseInput (str:string) =
    str.Split([|"\n";"\r\n"|],StringSplitOptions.RemoveEmptyEntries)
    |>Array.map parseRow
    |>Array.fold updateMapping (Map[])

let addMany collection set =
    collection 
    |> Seq.fold (fun s el -> Set.add el s) set

let calcReachable fromEl (graph:Map<int, int list>) =
    let rec calcInternal knownSoFar =
        let toAdd = 
            knownSoFar
            |> Set.fold 
                (fun s el -> addMany graph.[el] s)
                (Set[])
        
        if Set.isEmpty (toAdd - knownSoFar)
        then knownSoFar
        else calcInternal (Set.union toAdd knownSoFar)

    calcInternal (Set[fromEl])


let calcGroups graph =
    let keys = 
        graph 
        |> Map.toList 
        |> List.map fst
        |> Set.ofList
         
    let rec calc remainingKeys groupsSoFar =
        if Set.isEmpty remainingKeys
        then groupsSoFar
        else
            let from =remainingKeys |> Set.toList |> List.head
            let nextGroup = calcReachable from graph
            let newRemainingKeys = remainingKeys - nextGroup
            calc newRemainingKeys (nextGroup::groupsSoFar)

    calc keys []
let testInput = 
    """0 <-> 2
        1 <-> 1
        2 <-> 0, 3, 4
        3 <-> 2, 4
        4 <-> 2, 3, 6
        5 <-> 6
        6 <-> 4, 5"""
let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day12-1.txt"


let result1 =
    path
    |> IO.File.ReadAllText
    |> parseInput
    |> calcReachable 0
    |> Set.count    
let result2 =
    path
    |> IO.File.ReadAllText
    |> parseInput
    |> calcGroups
    |> List.length
