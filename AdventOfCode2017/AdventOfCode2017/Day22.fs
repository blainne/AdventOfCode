module Day22
open System

type Cell = | Infected | Clean | Weakened | Flagged
type Direction = | Up | Down | Left | Right

let turnLeft = function 
    | Up -> Left | Down -> Right
    | Left -> Down | Right -> Up

let turnRight = function 
    | Up -> Right | Down -> Left
    | Left -> Up | Right -> Down

let go (row, col) = function
    | Up -> (row - 1, col)
    | Down -> (row + 1, col)
    | Left -> (row, col - 1)
    | Right -> (row, col + 1)

let charToCell = function
    | '#' -> Infected
    | '.' -> Clean
    | _ -> failwith "Invalid input"

let parseInput (lines:string[]) =
    let map = 
        lines
        |> Array.map 
            ((Seq.map charToCell)>>Seq.indexed>>Array.ofSeq)
        |> Array.indexed
        |> Array.collect 
            (fun (row, cells) -> 
                cells |> 
                Array.map (fun (col, cell) -> ((row,col),cell)))
        |> Map.ofArray

    let startPoint = 
        ((Array.length lines)/2, (String.length (lines.[0])/2))

    (map, startPoint, Up)

let checkCell map point =
    if Map.containsKey point map
    then map.[point]
    else Clean

let calcNewState (map,currPoint,bearing,infections) =
    let newBearing, newCell, newInfections =
        match checkCell map currPoint with
        | Infected -> (turnRight bearing, Clean, infections)
        | Clean -> (turnLeft bearing, Infected, infections + 1)

    let newMap = Map.add currPoint newCell map
    let newPoint = go currPoint newBearing

    (newMap, newPoint, newBearing, newInfections)

let calcNewStateExtended (map,currPoint,bearing,infections) =
    let newBearing, newCell, newInfections =
        match checkCell map currPoint with
        | Infected -> (turnRight bearing, Flagged, infections)
        | Clean -> (turnLeft bearing, Weakened, infections)
        | Flagged -> ((turnLeft>>turnLeft) bearing, Clean, infections)
        | Weakened -> (bearing, Infected, infections + 1)
    let newMap = Map.add currPoint newCell map
    let newPoint = go currPoint newBearing

    (newMap, newPoint, newBearing, newInfections)


let rec makeStep stateFn n state=
    match n with 
    | 0 -> state
    | _ -> state |> stateFn |> makeStep stateFn (n-1)

let testPath =  "./AdventOfCode2017/AdventOfCode2017/inputs/Day22-test.txt" 
let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day22-1.txt"

let result1 =
    let map, startPoint, bearing =
        path 
        |> IO.File.ReadAllLines
        |> parseInput

    makeStep calcNewState 10000 (map, startPoint, bearing, 0)

let result2 =
    let map, startPoint, bearing =
        path 
        |> IO.File.ReadAllLines
        |> parseInput

    makeStep calcNewStateExtended 10000000 (map, startPoint, bearing, 0)