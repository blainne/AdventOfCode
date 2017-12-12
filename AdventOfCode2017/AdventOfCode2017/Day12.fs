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

let rec calcReachable 
let testInput = 
    """0 <-> 2
        1 <-> 1
        2 <-> 0, 3, 4
        3 <-> 2, 4
        4 <-> 2, 3, 6
        5 <-> 6
        6 <-> 4, 5"""
let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day12-1.txt"
parseInput testInput