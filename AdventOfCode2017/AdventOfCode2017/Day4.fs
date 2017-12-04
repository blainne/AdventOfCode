module Day4
open System.IO
open System


let buildWordMap words =
    let updateWithWord map w =
        let current =
            if (Map.containsKey w map) then map.[w] else 0
        Map.add w (current + 1) map
    
    words
    |> Array.fold updateWithWord (Map[]) 

let hasDuplicates map =
    map
    |> Map.filter (fun _ v -> v > 1) 
    |> Map.count
    |> (=) 0
let calc (inp:string []) =
    inp
    |> Array.map (fun line -> line.Split [|' '|])
    |> Array.map (buildWordMap)
    |> Array.filter (hasDuplicates>>not)
    |> Array.length

let input = File.ReadAllLines "./AdventOfCode2017/AdventOfCode2017/inputs/Day4-1.txt"

let result1 = calc input