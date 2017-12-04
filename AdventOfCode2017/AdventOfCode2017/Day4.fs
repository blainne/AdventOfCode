module Day4
open System.IO
open System

let updateWithWord map w =
    let current =
        if (Map.containsKey w map) then map.[w] else 0
    Map.add w (current + 1) map

let buildWordMap  =
    Array.fold updateWithWord (Map[]) 

let buildLetterSetMap (words:string[]) =
    words
    |> Array.map (Seq.sort>>String.Concat)
    |> Array.fold updateWithWord (Map[])

let hasDuplicates map =
    map
    |> Map.filter (fun _ v -> v > 1) 
    |> Map.count
    |> (<>) 0
let calc mapBuilder (inp:string []) =
    inp
    |> Array.map (fun line -> line.Split [|' '|])
    |> Array.map (mapBuilder)
    |> Array.filter (hasDuplicates>>not)
    |> Array.length

let input = File.ReadAllLines "./AdventOfCode2017/AdventOfCode2017/inputs/Day4-1.txt"
let input2 = [|"aa bb cc dd ee";"aa bb cc dd aa";"aa bb cc dd aaa"|]
let result1 = calc buildWordMap input
let result2 = calc buildLetterSetMap input