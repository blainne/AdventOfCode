module Day21
open System
open System.IO

let rotate = function
    | [a;b;c;d] -> [b;d;a;c]
    | [a;b;c;d;e;f;g;h;i] -> [c;f;i;b;e;h;a;d;g]
 
let flipH = function 
    | [a;b;c;d] -> [c;d;a;b]
    | [a;b;c;d;e;f;g;h;i] -> [g;h;i;d;e;f;a;b;c]

let flipV = function
    | [a;b;c;d] -> [b;a;d;c]
    | [a;b;c;d;e;f;g;h;i] -> [c;b;a;f;e;d;i;h;g]

let parseRule line = 
    line
    |> String.filter (fun c -> c<>'/')
    |> (fun s -> s.Split([|" => "|], StringSplitOptions.None))
    |> fun arr -> (List.ofSeq arr.[0], List.ofSeq arr.[1])

let parseInput = Array.map parseRule >> List.ofArray

let repeat n f =
    let rec recur n fn =
        match n with
        | 0 -> fn
        | _ -> recur (n-1) fn>>f
    
    recur n id

let expandRule mtx exp=
    let flips = [mtx; flipV mtx; flipH mtx]
    let rotations =
        [id; rotate; repeat 2 rotate; repeat 3 rotate]

    let newRules =
        [for m in flips do
            for f in rotations do
                yield f m]

    newRules |> List.map (fun r -> r,exp)

let expandRules rules =
    rules
    |> List.map (fun (r,exp) -> (expandRule r exp))
    |> List.collect id
    |> Map.ofList

let getSubMatrix mx my x y mtx =
    mtx
    |> List.skip (x*mx)
    |> List.take mx
    |> List.map ((List.skip (y*my))>>(List.take my))
    |> List.collect id

let toMatrices subSize sizeInSubMatrixes mtx =       
    [0..sizeInSubMatrixes-1]
    |> List.allPairs [0..sizeInSubMatrixes-1]
    |> List.map (fun (x,y) -> getSubMatrix subSize subSize x y mtx)

let rec toRows n rows matrix = 
    match matrix with
    | [] -> rows |> List.rev
    | _ -> toRows 
                n
                ((matrix |> List.take n)::rows) 
                (matrix |> List.skip n)

let divide mtx =
    let mtxSize =
        mtx |> List.length |> float |> Math.Sqrt |> int
    let subMatrixSize = 
        if mtxSize % 2 = 0 then 2 else 3
    let sizeInSubMatrixes = mtxSize/subMatrixSize
    
    let divided =
        mtx 
        |> toRows mtxSize []
        |> toMatrices subMatrixSize sizeInSubMatrixes
    
    (divided, sizeInSubMatrixes)

let flattenRows subSize sizeInSubMatrixes mtx =    
    [0..subSize-1]
    |> List.allPairs [0..sizeInSubMatrixes-1]
    |> List.map (fun (x,y) -> getSubMatrix sizeInSubMatrixes subSize x y mtx)

let flatten (mtxs, sizeInSubMatrixes) =
    let subSize =
        mtxs |> List.head |> List.length |> float |> Math.Sqrt |> int
    
    mtxs 
    |> flattenRows subSize sizeInSubMatrixes 
    |> List.collect id

let path1 = "./AdventOfCode2017/AdventOfCode2017/inputs/Day21-1.txt"

//should be 155
let path2 = "./AdventOfCode2017/AdventOfCode2017/inputs/Day21-2.txt"
//should be 208
let path3 = "./AdventOfCode2017/AdventOfCode2017/inputs/Day21-3.txt"
let result1 = 
    let rules = 
        path1
        |> File.ReadAllLines
        |> parseInput
        |> expandRules 

    let initial = ".#...####" |> List.ofSeq
    let workflow mtx _ =
            mtx 
            |> divide
            |> (fun (mat,s) -> 
                    (List.map (fun m -> rules.[m]) mat,s))
            |> flatten

    [1..18]
    |> List.fold workflow initial
    |> List.where ((=) '#')
    |> List.length

