module Day6 
open System

[<Measure>]
type blocks

let preprocessInput (inp:string) = 
    inp.Split [|' ';'\t'|] 
    |> Array.map (int>>((*) 1<blocks>))


let incrCell i (arr: int<blocks>[]) =
    arr.[i] <- arr.[i] + 1<blocks>

let makeKey arr =
    let strs = arr |> Array.map string
    String.Join ("_", strs)    
            
let rec distribute (arr: int<blocks>[]) (currIndex,blockCount) =
    if blockCount = 0<blocks>
    then arr
    else
        let nextIndex = (currIndex + 1) % arr.Length
        do incrCell nextIndex arr
        distribute arr (nextIndex, blockCount - 1<blocks>)

let findMostBlockedBank arr =
    arr 
    |> Array.mapi (fun i e -> (i,e))
    |> Array.maxBy (fun (i,e) -> e)

let calc arr =
    let rec internalCalc count (map:Map<string, int>) arr =
        let key = makeKey arr
        if map.ContainsKey key
        then (count, count - map.[key])
        else 
            let set' = map.Add (key, count)
            let (i, el) = findMostBlockedBank arr
            arr.[i] <- 0<blocks>
            let arr' = distribute arr (i,el)
            internalCalc (count+1) set' arr' 

    internalCalc 0 (Map[]) arr 

let input = "0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11"

let result1and2 = 
    input
    |> preprocessInput
    |> calc      
