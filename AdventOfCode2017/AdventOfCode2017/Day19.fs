module Day19
open System

type RoadSegment = | Normal | Symbol of char | Empty | Crossing

let parseChar = function 
    | '|' | '-' -> Normal
    | ' ' -> Empty
    | '+' -> Crossing
    | c -> Symbol c

let parseLines= 
    Array.map (Seq.map parseChar >> Array.ofSeq)

let findStart (roads: RoadSegment[][]) =
    let x = 
        roads.[0]
        |> Array.findIndex (function | Normal -> true | _ -> false)
    
    (0,x,(1,0))

let followBearing (row,col,bearing) =
    (row + fst bearing, col + snd bearing, bearing)

let searchAdjacent (roads: RoadSegment[][]) (row,col,(br,bc)) =
    let checkBearing (brow, bcol) =
        if (roads.[row+brow].[col+bcol]) <> Empty
        then Some(brow,bcol)
        else None
    
    [(0,1);(0,-1);(1,0);(-1,0)]
    |> List.where (fun b -> b<>(-br, -bc))
    |> List.map (checkBearing)
    |> List.tryPick (id)

let rec processPos (roads: RoadSegment[][])  steps currPos symbols =
    let recur = processPos roads (steps + 1)
    let (row, col, bearing) = currPos
    match roads.[row].[col] with
    | Empty -> failwith "Shouldn't happen"
    | Normal -> 
        recur 
            (followBearing currPos)
            symbols
    | Symbol s -> 
        if searchAdjacent roads currPos = None
        then (s::symbols, steps)
        else
            recur 
                (followBearing currPos)
                (s::symbols)
    | Crossing ->
        match searchAdjacent roads currPos with
        | Some(newBearing) -> 
                recur 
                    (followBearing (row, col, newBearing))
                    symbols
        | None -> failwith "Shouldn't happen"
let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day19-1.txt"
let testPath = "./AdventOfCode2017/AdventOfCode2017/inputs/Day19-test.txt"

let result1and2 =
    let roads =
        path
        |> IO.File.ReadAllLines
        |> parseLines
    
    let (symbols, steps)
     = processPos roads 1 (findStart roads) []

    (symbols |> List.rev |> List.toArray |> String,
     steps)
    