module Day16
open System.Collections.Generic

type Instr =
    | Spin of int
    | Exchange of int*int
    | Partner of char*char

let parseInstruction (str:string) =
    match str.[0] with
    | 's' -> Spin(str.Substring(1) |> int)
    | 'x' -> 
        let tokens = str.Substring(1).Split([|'/'|])
        Exchange(int tokens.[0], int tokens.[1])
    | 'p' -> Partner(str.[1],str.[3])

let parseInput (str:string) =
    str.Split [|','|]
    |> Array.map parseInstruction
    |> Array.toList

let processSpin (Spin(n)) map =
    Map.map (fun k ind -> (ind + n)%(Map.count map)) map

let processExchange (Exchange(i1,i2)) map =
    let pickByIndex i = 
        map |> Map.pick (fun k v -> if(v = i) then Some(k) else None)
    let k1 = pickByIndex i1
    let k2 = pickByIndex i2
    
    map
    |> Map.add k1 i2
    |> Map.add k2 i1

let processPartner (Partner(key1,key2)) map =
    map
    |> Map.add key1 map.[key2]
    |> Map.add key2 map.[key1]

let processInstruction map instr=
    match instr with
    | Spin(_) -> processSpin instr map
    | Exchange(_) -> processExchange instr map
    | Partner (_) -> processPartner instr map

let calcResult instructions initialOrder = 
    let initialMap =
        initialOrder
        |> Seq.mapi (fun i l -> (l,i))
        |> Map.ofSeq

    instructions
    |> List.fold processInstruction initialMap
    |> Map.toArray
    |> Array.sortBy snd
    |> Array.map fst
    |> System.String

let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day16-1.txt"

let instructions = 
    path
    |> System.IO.File.ReadAllText
    |> parseInput

let result1 = 
    calcResult instructions "abcdefghijklmnop"

let result2 =
    let initial = "abcdefghijklmnop"
    let calc = calcResult instructions

    let rec checkCycle x i =
        if x = initial
        then i 
        else checkCycle (calc x) (i+1)

    let cycle = checkCycle (calc initial) 1
    
    seq[1..(1_000_000_000%cycle)]
    |> Seq.fold 
        (fun inp _ -> calc inp)
        initial