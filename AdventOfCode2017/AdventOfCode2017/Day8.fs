module Day8
open System.IO
open System

type Operation = | Incr | Decr
type Condition = | Eq | Neq | Leq | Geq | Lt | Gt
type Argument = | Reg of string | Const of int
type Expr<'a> = {Op: 'a; LeftArg: Argument; RightArg: Argument}
type Instruction = {Op: Expr<Operation>; Cond: Expr<Condition>}

let parseErr () = failwith "Unexpected input"

let parseOperation (tokens:string[]) =
        let registryName = tokens.[0]
        let amount = int tokens.[2]
        let op = 
            match tokens.[1] with
            | "inc" -> Incr | "dec" -> Decr | _ -> parseErr()
        {Op = op; LeftArg = Reg(registryName); RightArg = Const(amount)}

let parseCondition (tokens:string[]) =
        let registryName = tokens.[4]
        let amount = int tokens.[6]
        let cond = 
            match tokens.[5] with
            | "<" -> Lt | ">" -> Gt | "==" -> Eq
            | "!=" -> Neq | ">=" -> Geq | "<=" -> Leq
            | _ -> parseErr()

        {Op = cond; LeftArg = Reg(registryName); RightArg = Const(amount)}
        
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

// let runInstruction regs instr =
//     let (condRegistry, condVal) = instr.Cond
    

// let execute instructions =
//     instructions
//     |> List.fold runInstruction (Map[])


let testInput = "./AdventOfCode2017/AdventOfCode2017/inputs/Day8-test.txt"