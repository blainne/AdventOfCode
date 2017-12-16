open System.Web.UI.WebControls
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

let bitRep n =
    let rec calc num bits =
        if num = 0 then bits
        else calc (num>>>1) ((num%2)::bits)
    
    calc n []

let bitSums = 
    Array.map 
        (fun b -> 
            HammingWeights.[b/16] 
            + HammingWeights.[b%16])
    >> Array.sum

let inputCore = "ugkiagan" + "-"
let result1 = 

    [0..127]
    |> List.fold 
        (fun sum i ->
            let h = hash (inputCore + (i.ToString()))
            sum + (bitSums h))
        0

let result2 = 
    let hashes = 
        [0..127]
        |> List.map hash (inputCore + (i.ToString()))