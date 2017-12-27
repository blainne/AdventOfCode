module Day24
open System.IO

type Module = 
    { Port1:int; Port2:int; Uid:int }
        member this.Strength = this.Port1 + this.Port2  

type Tree = | Node of Module * Set<Tree> | Leaf of Module

let rec cata f fLeaf = function
    | Node (m, subs) -> f m (subs |> Set.map (cata f fLeaf))
    | Leaf m -> fLeaf m 

let parseLine n (line:string) =
    line.Split '/' 
    |> fun arr -> 
        {
            Port1 = int arr.[0]; 
            Port2 = int arr.[1]; 
            Uid = n
        }

let parseInput (lines:string[]) = 
    lines |> Array.mapi parseLine |> Set.ofArray

let freePort modl takenPort = 
    if modl.Port1 = takenPort
    then modl.Port2
    else modl.Port1

let hasPort modl port =
   modl.Port1 = port || modl.Port2 = port

let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day24-1.txt"
let testpath = "./AdventOfCode2017/AdventOfCode2017/inputs/Day24-test.txt"

let startingModule = { Port1=0; Port2=0; Uid = -1}

let rec buildTree port root modules =
    let joinPort = freePort root port 
    let subtrees = 
        modules 
        |> Set.filter (fun m -> hasPort m joinPort)
        |> Set.map (fun m -> buildTree (joinPort) m (modules - Set[m]))    
        
    if Set.isEmpty subtrees    
    then Leaf root
    else Node(root, subtrees)

let findStrongestPath tree =
    let updateStrength (modl:Module) subPaths = 
        modl.Strength + (Set.maxElement subPaths)

    let leafStrength (m:Module) = m.Strength

    tree |> cata updateStrength leafStrength

let pathStrength path =
    List.fold (fun str (m:Module) -> str + m.Strength) 0 path

let getAllPaths tree =
     let addToPath modl subPaths =
        subPaths
        |> Set.map (fun paths -> 
            paths |> List.map 
                (fun p ->modl::p))
        |> Set.toList
        |> List.collect id
     
     let makePath m = [[m]]

     tree |> cata addToPath makePath

let result1 = 
    let modules = 
        path
        |> File.ReadAllLines 
        |> parseInput
    
    modules
    |> buildTree 0 startingModule 
    |> findStrongestPath

let result2 = 
    let modules = 
        path
        |> File.ReadAllLines 
        |> parseInput
    
    modules
    |> buildTree 0 startingModule 
    |> getAllPaths
    |> List.groupBy List.length
    |> List.maxBy (fun (len, _) -> len)
    |> snd
    |> List.maxBy (pathStrength)
    |> pathStrength
