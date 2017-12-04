module Day3

let sameDirectionSteps = 
    Seq.unfold 
        (fun (prev, repeat) -> 
            let nextRepeat = (repeat + 1) % 2
            let next = prev + nextRepeat
            Some(next, (next, nextRepeat)))
        (0, 0)

type Direction = | Left | Right | Up | Down
let rec subsequentDirections =    
    seq {
        yield! [Right; Up; Left; Down]
        yield! subsequentDirections
    }

let directionSteps =
    Seq.zip sameDirectionSteps subsequentDirections
    |> Seq.collect (fun (n,dir) -> Seq.replicate n dir)

type IndexModifier = {x:int; y:int}

type MemPos = {value:int; x:int; y:int}

let dirToIndexMod =
    function
    | Left -> {x = -1; y = 0}
    | Right -> {x = 1; y = 0}
    | Up -> {x = 0; y = 1}
    | Down -> {x = 0; y = -1}

let genNextSpiralStep (h::t) dir =
    let indexMod = dirToIndexMod dir
    let newElem = 
        {value = h.value + 1; 
        x = h.x + indexMod.x;
        y = h.y + indexMod.y}
    newElem::h::t
let spiral size = 
    directionSteps
    |> Seq.take (size - 1)
    |> Seq.fold 
        genNextSpiralStep 
        [{value = 1; x = 0; y = 0}]

let calcElemValue2 map (x,y)=
    map
    |> Map.filter (fun (ix,iy) _ -> abs(ix - x) <=1 && abs(iy - y) <=1)
    |> Map.toList
    |> List.sumBy (snd>>(fun mPos -> mPos.value))
 

let genNextSpiralStep2 (prev, posMap) dir =
    let indexMod = dirToIndexMod dir
    let newx = prev.x + indexMod.x
    let newy = prev.y + indexMod.y
    let newMemPos = 
        { value = calcElemValue2 posMap (newx, newy);
          x = newx;
          y = newy;}
    
    (newMemPos, Map.add (newx, newy) newMemPos posMap)

let findProblem2 inputValue = 
    let initialList = directionSteps |> Seq.take (inputValue - 1) |> List.ofSeq
    let rec innerFind (dir::rest) mapSoFar prev =
          let (v, newMap) = genNextSpiralStep2 (prev, mapSoFar) dir
          if v.value > inputValue
          then v
          else innerFind rest newMap v
    
    innerFind initialList (Map[((0,0), {value = 1; x = 0; y = 0})]) {value = 1; x = 0; y = 0}

let result1 = 
    spiral 347991
    |> List.head
    |> fun memPos -> abs(memPos.x) + abs(memPos.y)

let result2 = 
    findProblem2 347991
