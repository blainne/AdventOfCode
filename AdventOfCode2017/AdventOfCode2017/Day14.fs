module Day14

let movePos curPos n arr = (curPos+n)%(Array.length arr)
let nextPos curPos = movePos curPos 1

let getReversedSlice start length (arr:int[]) =
    let rec internalSlice listSoFar i sliceLen =
        match sliceLen with
        | 0 -> listSoFar
        | _ -> internalSlice 
                    (arr.[i]::listSoFar)
                    (nextPos i arr)
                    (sliceLen - 1)
    
    (internalSlice [] start length)

let updateWithSlice start slice (arr:int[]) =
    let rec update i sliceLeft =
        match sliceLeft with
        | [] -> ()
        | h::rest -> 
            arr.[i] <- h
            update (nextPos i arr) rest     

    update start slice

let processStep (arr,skip,i) currLen =
    let slice = getReversedSlice i currLen arr
    updateWithSlice i slice arr
    let newPos = movePos i (currLen+skip) arr
    (arr, skip + 1, newPos)

let calc arr lengths =
    lengths 
    |> Seq.fold processStep (arr, 0, 0)

let baseArr = [0..255]
let replicate n s =
    seq[for i in 1..n do yield! s]

let hash inputStr =
    let lengths =
        inputStr
        |> Seq.map int
        |> fun s -> Seq.append s [17; 31; 73; 47; 23]
        |> replicate 64

    let sparseHash,_,_ = calc (Array.ofList baseArr) lengths

    sparseHash
    |> Array.chunkBySize 16
    |> Array.map (Array.reduce (^^^))

let HammingWeights =
    [|0;1;1;2;1;2;2;3;1;2;2;3;2;3;3;4|]

type RegionSymbol = | Zero | Bit | Region of int 

let bitRep padding n =
    let rec calc num bits len =
        if num < 2 then (num::bits, len + 1)
        else calc (num>>>1) ((num%2)::bits) (len + 1)
    
    let padZeros (list,listLen) =
        list
        |> List.append (List.init (padding - listLen) (fun _ -> 0))

    calc n [] 0
    |> padZeros
    |> Array.ofList 
    |> Array.map (function | 0 -> Zero | 1 -> Bit | _ -> failwith "")


let hashToBits padding = Array.collect (bitRep padding)


let bitSums = 
    Array.map 
        (fun b -> 
            HammingWeights.[b/16] 
            + HammingWeights.[b%16])
    >> Array.sum
let maxHashI = 127
let inputCore = "ugkiagan" + "-"
let result1 = 
    [0..maxHashI]
    |> List.fold 
        (fun sum i ->
            let h = hash (inputCore + (i.ToString()))
            sum + (bitSums h))
        0

let rec markRegion 
        (x,y) 
        regNum 
        (arr:RegionSymbol [] []) =
    if x < 0 || y < 0 || x >= arr.Length || y>= arr.Length 
    then (arr,false)
    else
        match arr.[x].[y] with
        | Zero -> (arr,false) | Region(_) -> (arr,false)
        | Bit -> 
            arr.[x].[y] <- Region(regNum)
            
            markRegion (x-1, y) regNum arr
            markRegion (x, y-1) regNum arr
            markRegion (x+1, y) regNum arr
            markRegion (x, y+1) regNum arr

            (arr, true)




let result2 = 
    let hashes = 
        [0..maxHashI]
        |> List.map (fun i -> hash (inputCore + (i.ToString())))
        |> Array.ofList 
        |> Array.map (hashToBits 8)

    let indices = 
        seq {for x in 0..maxHashI do
             for y in 0..maxHashI do
             yield x,y}
    
    let num, regions = 
        indices
        |> Seq.fold 
                (fun (regNum,arr) pos -> 
                 let newArr, foundNewReg = markRegion pos regNum arr
                 let newRegNum = 
                    if foundNewReg then regNum + 1 else regNum         
                 (newRegNum, newArr))
            (0, hashes)

    num
