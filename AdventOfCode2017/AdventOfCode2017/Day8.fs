module Day8
open System.IO
open System

type Operation = | Incr of string*int | Decr of string*int
type Condition = 
    | Eq of string*int 
    | Neq of string*int 
    | Leq of string*int
    | Geq of string*int
    | Lt of string*int
    | Gt of string*int

type Instruction = {Op: Operation; Cond: Condition}

let parseOperation (tokens:string[]) =
        let registryName = tokens.[0]
        let amount = int tokens.[2]
        match tokens.[1] with
        | "inc" -> Incr(registryName, amount)
        | "dec" -> Decr(registryName, amount)
        | _     -> failwith "Unexpected input"

let parseCondition (tokens:string[]) =
        let registryName = tokens.[4]
        let amount = int tokens.[6]
        match tokens.[5] with
        | "<" -> Lt(registryName, amount)
        | ">" -> Gt(registryName, amount)
        | "==" -> Eq(registryName, amount)
        | "!=" -> Neq(registryName, amount)
        | ">=" -> Geq(registryName, amount)
        | "<=" -> Leq(registryName, amount)
        | _     -> failwith "Unexpected input"
let parseRow (str:string) =
    let tokens = str.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    { Op = parseOperation tokens; Cond = parseCondition tokens }

let parseInput path =
    File.ReadAllLines path
    |> Array.map parseRow
    |> List.ofArray

let readAndInit key map = 
    if Map.containsKey key map
    then (map.[key], map)
    else (0, Map.add key 0 map) 

let runInstruction regs instr =
    let (condRegistry, condVal) = instr.Cond
    

let execute instructions =
    instructions
    |> List.fold runInstruction (Map[])


let testInput = "./AdventOfCode2017/AdventOfCode2017/inputs/Day8-test.txt"