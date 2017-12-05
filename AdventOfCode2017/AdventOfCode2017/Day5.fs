module Day5
open System.IO

let prepareInput path = 
    path
    |> File.ReadAllLines
    |> Array.map int

let processLocalJump1 (arr:int[]) steps pos =
    let jumpDist = arr.[pos]
    do arr.[pos] <- arr.[pos] + 1
    (arr,(steps + 1),(pos + jumpDist))

let processLocalJump2 (arr:int[]) steps pos =
    let jumpDist = arr.[pos]
    do arr.[pos] <- if jumpDist > 2 then jumpDist - 1 else jumpDist + 1
    (arr,(steps + 1),(pos + jumpDist))

let calc stepLogic (input:int[]) = 
    let rec jump (arr:int[]) steps pos = 
        if (pos + arr.[pos]) >= arr.Length 
        then steps + 1
        else 
            let (newArr, newSteps, newPos) = stepLogic arr steps pos
            jump newArr newSteps newPos

    jump input 0 0

let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day5-1.txt"

let result1 = (prepareInput>>(calc processLocalJump1)) path
let result2 = (prepareInput>>(calc processLocalJump2)) path

