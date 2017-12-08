module Day7
open System.IO

type Node = 
    {
        name : string;
        weight : int;
        children : string list
    }

let (>>=) o f = Option.bind f o
let (<*>) o f = Option.map f o

let valOrDefault defaultVal key map = 
    if Map.containsKey key map
    then map.[key]
    else defaultVal

let valOrZero = valOrDefault 0

let parseRow (row:string) =
    let readName (tok::rest) =
        ({name = tok; children = []; weight = 0}, rest)
    
    let readWeight ((tkn, tokens):Node * string list) =
        let tok::rest = tokens
        let weightStr = tok.Substring(1, (tok.Length - 2))
        {tkn with weight = int weightStr}, rest
    
    let readChildren ((tkn, tokens):Node * string list) = 
        if List.isEmpty tokens
        then tkn
        else {tkn with children = (List.tail tokens)}
    
    row.Split ([|' ';','|], System.StringSplitOptions.RemoveEmptyEntries)
    |> (List.ofArray>>readName>>readWeight>>readChildren)

let updateMapWithNames map (node:Node) =
    let updateKey map key = 
        let value = valOrZero key map
        Map.add key (value + 1) map

    let updated = updateKey map (node.name)
    node.children
    |> List.fold updateKey updated


let parseInput path = 
    path
    |> File.ReadAllLines
    |> Array.map parseRow
   

let findRoot = Map.pick (fun name count -> 
                    if count = 1 
                    then Some(name) 
                    else None)

let calc1 path =
    path
    |> parseInput
    |> Array.fold updateMapWithNames (Map[])
    |> findRoot


type Balance = 
   | Good of int * int
   | Imbalanced of Balance list
let rec foldBackMapTree 
        folder 
        init 
        (map : Map<string, Node>)
        rootKey =
    let node = map.[rootKey]

    if List.isEmpty node.children
    then Good(node.weight, init)
    else 
        let childResults = 
            node.children
            |> List.map (fun c ->foldBackMapTree folder init map c) 
        folder childResults node

let pickWhereFst v list =
    List.tryPick (function | (x,el) when x = v -> Some(el) | _ -> None) list

let all cond list =
    list |> List.fold (fun sofar el -> if cond el then sofar && true else false) true

let calcBalance childResults node =
    let childrenSum = childResults |> List.pick (function | Good(n,sum) -> Some(n+sum) | _ -> None)

    if childResults |> all (function | Good (n,sum)-> n+sum = childrenSum | _ -> false)
    then Good(node.weight, List.sumBy (function | Good (n,sum)-> n+sum | _ -> 0 ) childResults)
    else Imbalanced(childResults)

let rec findInnermostImbalance balance = 
    match balance with
    | Good(_) -> None
    | Imbalanced(list) ->
        let inner = List.tryFind (function | Imbalanced(_) -> true | Good(_) -> false) list
        match inner with
        | None -> Some(Imbalanced(list))
        | Some(i) -> findInnermostImbalance i

let stripGoodDiscriminators list =
    let addIfGood (tuples,soFar) balance =                    
        match balance, soFar with
        | Good(a,b), true -> ((a,b)::tuples, true)
        | _ -> (tuples, false)

    let nums, ok = List.fold addIfGood ([], true) list
    if ok then Some(nums) else None                
    
let calc2 path =
    let parsed =     
        path
        |> parseInput
 
    let rootName = 
        parsed
        |>Array.fold updateMapWithNames (Map[])
        |>findRoot

    let map = 
        parsed 
        |> Array.map (fun node -> node.name, node)
        |> Map.ofArray

    let balance = foldBackMapTree calcBalance 0 map rootName

    findInnermostImbalance balance
    >>= (function | Imbalanced (list) -> Some(list) | _ -> None)
    >>= stripGoodDiscriminators
    <*> List.groupBy (fun (a,b) -> a+b)
    <*> (fun list -> 
            let imbaRoot, imbaChildren = ((List.minBy (snd>>List.length))>>snd>>List.head) list
            let goodRoot, goodChildren = ((List.maxBy (snd>>List.length))>>snd>>List.head) list
            imbaRoot + (goodRoot + goodChildren - imbaRoot - imbaChildren))
   
let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day7-1.txt"
let result1 = calc1 path
let result2 = calc2 path