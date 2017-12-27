module Day23
open System.IO

let readRegistry c map =
    if Map.containsKey c map
    then map.[c]
    else 0L 

let update map key fVal  =
    let currVal = readRegistry key map
    map |> Map.add key (fVal currVal)

type ValueSource =
| Registry of char
| Const of int64

    member this.GetVal map =
        match this with
        | Const(x) -> x
        | Registry(c) -> readRegistry c map

type Operation =
| Sub of char*ValueSource
| Mul of char*ValueSource
| Set of char*ValueSource
| Jnz of ValueSource*ValueSource

let (|IsConst|_|) (str:string) = 
   match System.Int64.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

let (|IsRegistry|_|)(str:string) =
   match System.Char.TryParse(str) with
   | (true,c) -> Some(c)
   | _ -> None

let parseVS token = 
    match token with
    | IsConst(c) -> Const(c)
    | IsRegistry(r) -> Registry(r)

let parseOperation (line:string) = 
    let tokens = line.Split [|' '|]
    match tokens.[0] with
    | "sub" -> Sub(char tokens.[1], parseVS tokens.[2])
    | "set" -> Set(char tokens.[1], parseVS tokens.[2])
    | "mul" -> Mul(char tokens.[1], parseVS tokens.[2])
    | "jnz" -> Jnz(parseVS tokens.[1], parseVS tokens.[2])

let parseInput lines = lines |> Array.map parseOperation

let makeArithmOper map oper =
    match oper with
    | Mul(x,y) -> update map x ((*) (y.GetVal map))
    | Set(x,y) -> update map x (fun _ -> y.GetVal map)
    | Sub(x,y) -> update map x (fun x -> x - (y.GetVal map))
    
let rec processStep (ops:Operation[]) map i mulCount= 
    if (i = 10)
    then printfn "%A" (i, map)
    else ()
    if i >= Array.length ops
    then (map, mulCount)
    else
        match ops.[i] with
        | Set(_) | Sub(_) ->
            processStep ops
                (makeArithmOper map (ops.[i]))
                (i+1)
                mulCount
        | Mul(_) -> 
            processStep ops
                (makeArithmOper map (ops.[i]))
                (i+1)
                (mulCount + 1)            
        | Jnz(x,y) ->
            let jump = 
                if (x.GetVal map) <> 0L
                then y.GetVal map |> int
                else 1
            processStep ops map (i+jump) mulCount

let countMultiplications (ops:Operation[]) =
    processStep ops (Map<char,int64>[]) 0 0 |> snd


let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day23-1.txt"
let result1 = 
    path
    |> File.ReadAllLines
    |> parseInput
    |> countMultiplications 

let result2 = 
    let inner (b:int64) =
        [2L..(float>>System.Math.Sqrt>>int64>>((+) 1L)) b]
        |> List.exists (fun e -> b % e = 0L)

    [109300L..17L..126300L]
    |> List.filter inner
    |> List.length

//code for result 2 is effect of manually translating assembly code to pseudo code and then to f#
//my pseudo code:
// b <- 109300
// c <- 126300
// do{
//     f <- 1
//     d <- 2
//     do {
//         e <- 2
//         do{           
//             if d*e = b
//                 f <- 0
//             e++ 
//         } while e<>b

//         d++
//     } while d<>b
//     if f == 0
//         h++

//     if b == c
//         end
//     b += 17
// } while(true)