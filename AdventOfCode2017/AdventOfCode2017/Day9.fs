module Day9
open System.IO

type State = 
    | Group of int | Garbage | Ignoring 

let reactInGroupState nesting stack total gbChars ch = 
    let rest = List.tail stack
    match ch with
    | '}' -> rest, (total + nesting), gbChars
    | '{' -> Group(nesting + 1)::stack, total, gbChars
    | '<' -> Garbage::stack, total, gbChars
    |  _ -> stack, total, gbChars
    
let reactInGarbageState stack total gbChars ch = 
    let rest = List.tail stack
    match ch with
    | '>' -> rest, total, gbChars
    | '!' -> Ignoring::stack, total, gbChars
    |  _ -> stack, total, (gbChars+1)
    
let reactInIgnoringState stack total gbChars ch = 
    (List.tail stack, total, gbChars)

let react (stack,total,gbChars) ch =
    let curr::rest = stack
    match curr with
    | Ignoring -> reactInIgnoringState stack total gbChars ch
    | Garbage -> reactInGarbageState stack total gbChars ch
    | Group(n) -> reactInGroupState n stack total gbChars ch 

let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day9-1.txt"

let processStream str =
    str
    |> Seq.fold react ([Group(0)],0,0)

let result1 = 
    path
    |> File.ReadAllLines
    |> Array.map (processStream)
       


