module Day11
open System.IO
let movement = 
    Map[("n",(2,0)); ("ne",(1,-1));("nw",(1,1));
        ("s",(-2,0));("se",(-1,-1));("sw",(-1,1))]


let addMovement (v1,h1) (v2,h2) = (v1+v2, h1+h2)

let calcTargetPos = List.fold (addMovement) (0,0)

let processInput (str:string) =
    str.Split[|','|]
    |> Array.map (fun s -> movement.[s])
    |> List.ofArray

let calc1 input =
    let (posRow, posCol) = 
        input 
        |> processInput
        |> calcTargetPos
    
    let verticalsRequired = 
        match abs(posRow) - abs(posCol) with
        | x when x > 0 -> x/2
        | _ -> 0
    
    abs(posCol) + verticalsRequired
    
let result1 = 
    let input = 
         "./AdventOfCode2017/AdventOfCode2017/inputs/Day11-1.txt"
         |> File.ReadAllText

    calc1 input