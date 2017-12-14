module Day13
open System

let parseInput(lines: string []) = 
    lines
    |> Array.fold 
        (fun (map, maxLayer) line -> 
            let tokens = line.Split([|": "|], StringSplitOptions.RemoveEmptyEntries)
            let layer = int tokens.[0]
            let newMap = Map.add layer (int tokens.[1])  map
            (newMap, max layer maxLayer))
        (Map[],0)

type MoveDir = | Up | Down
type Scanning = | Cell of int*MoveDir | None

let initScanner key map = 
    match Map.tryFind key map with
    | Some(_) -> Cell(0, Down)
    | Option.None -> Scanning.None

let initFirewall (scannerCfg, noOfLayers) =
    Array.init 
        (noOfLayers+1)
        (fun i -> initScanner i scannerCfg )

let updateFirewall (scannerCfg:Map<int, int>) firewall =
    let updateLayer i =
        function 
        | None -> None 
        | Cell(v, dir) -> 
            let limit = scannerCfg.[i]
            let newDir, dirModifier = 
                match v, dir with
                | 0, Up -> Down, 1
                | x, Down when x = (limit-1) -> Up, -1
                | _, Up -> Up, -1
                | _, Down -> Down, 1
            Cell(v+(dirModifier), newDir)
    
    Array.mapi updateLayer firewall
        
let findCaughtSpots firewall (scannerCfg, noOfLayers) =
    [0..noOfLayers]
    |> List.fold 
        (fun ((fw:Scanning[]), caughtIn) step ->
            let newPositions =
                match fw.[step] with
                | Cell(0, _) -> step::caughtIn
                | _ -> caughtIn
            (updateFirewall scannerCfg fw, newPositions))
        
        (firewall, [])

let testInput = 
    """0: 3
    1: 2
    4: 4
    6: 4"""
    
let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day13-1.txt"

let result1 = 
    let (cfg, max) = parseInput (IO.File.ReadAllLines path)
    let fw = initFirewall (cfg,max)

    findCaughtSpots fw (cfg,max)
    |> snd
    |> List.sumBy (fun e -> e * cfg.[e])


let rec calcWaitSteps firewall (cfg,max) waitStep=
    let _, caughtIn = findCaughtSpots firewall (cfg,max) 
    match caughtIn with 
    | [] -> waitStep
    | _ -> 
        let newFw = updateFirewall cfg firewall
        calcWaitSteps newFw (cfg,max) (waitStep + 1)

let result2 = 
    let (cfg, max) = parseInput (IO.File.ReadAllLines path)
    let fw = initFirewall (cfg,max)
    calcWaitSteps fw (cfg,max) 0
