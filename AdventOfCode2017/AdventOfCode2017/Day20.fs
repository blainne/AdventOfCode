module Day20
open System

type Vector = {X:int64; Y:int64; Z:int64}
    with 
        member this.Taxi () = (abs this.X) + (abs this.Y) + (abs this.Z)
        member this.Add v = {X = this.X + v.X; Y = this.Y + v.Y; Z = this.Z + v.Z}
        member this.Distance v = (abs (this.X - v.X)) + (abs (this.Y - v.Y)) + (abs (this.Z - v.Z))

type Point = {Pos:Vector; V:Vector; A:Vector}

let parseVector (str:string) =
    let tokens = 
        str.Substring(3, str.Length - 4).Split [|','|]
        |> Array.map int64    
    
    {X = tokens.[0]; Y = tokens.[1]; Z= tokens.[2] }
    
let parseLine (str:string)= 
    let tokens = 
        str.Split([|", "|], StringSplitOptions.None)
    
    let pos = parseVector ((tokens.[0]).Trim())
    let vel = parseVector ((tokens.[1]).Trim())
    let acc = parseVector ((tokens.[2]).Trim())

    {Pos = pos; V = vel; A=acc}

let makeStep point =
    let newVel = point.V.Add(point.A)
    {point with 
        Pos = point.Pos.Add(newVel); 
        V = point.V.Add(point.A)}

let areGettingAwayFromEachOther (v1,v2) =
    let nextV1 = makeStep v1
    let nextV2 = makeStep v2

    let distNow = v1.Pos.Distance(v2.Pos)
    let vDiffNow = v1.V.Distance(v2.V)
    let distNext = nextV1.Pos.Distance(nextV2.Pos)
    let vDiffNext = nextV1.V.Distance(nextV2.V)

    distNow < distNext && vDiffNow <= vDiffNext

let noMoreCollisions pts=
    let points = List.indexed pts
    
    points 
    |> List.allPairs points
    |> List.filter (fun ((i1,_),(i2,_)) -> i1 <> i2)
    |> List.map (fun ((_,p1),(_,p2)) -> (p1,p2))
    |> List.forall areGettingAwayFromEachOther

let mapT f (a,b,c) = (f a, f b, f c)

let comparer (_,v1) (_,v2) =
    let (pos1, vel1, acc1) = mapT abs v1
    let (pos2, vel2, acc2) = mapT abs v2

    let result = 
        if acc1 <> acc2 then acc1 - acc2 
        else if vel1<>vel2 then vel1 - vel2
        else if pos1 <> pos2 then pos1 - pos2
        else 0L
    
    if result > 0L 
    then 1
    else if result < 0L
    then -1
    else 0

        
let rec recalcCollisions i points =
    let newPoints = 
        points 
        |> List.map makeStep
        |> List.groupBy (fun (v) -> v.Pos)
        |> List.filter (fun (_, points) -> points.Length = 1)
        |> List.collect ( fun (_, points) -> points)

    if(i%1000 = 0 && noMoreCollisions newPoints)
    then newPoints
    else recalcCollisions (i+1) newPoints

let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day20-1.txt"

let result1 = 
    IO.File.ReadAllLines path
    |> Array.mapi (fun i v -> (i, parseLine v))
    |> Array.map (fun (i,v) -> (i, (v.Pos.Taxi(), v.V.Taxi(), v.A.Taxi())))
    |> Array.sortWith comparer
    |> Array.item 0

let result2 =
    let points = 
        IO.File.ReadAllLines path
        |> List.ofArray
        |> List.map (fun v -> (parseLine v))       

    recalcCollisions 0 points |> List.length