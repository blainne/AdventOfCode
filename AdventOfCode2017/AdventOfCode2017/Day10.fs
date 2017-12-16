module Day10

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


let replicate n s =
    seq[for i in 1..n do yield! s]

let testLengths = [3;4;1;5]
let testArr = [|0..4|]
let baseArr = [0..255]
let inputStr = "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24"


let result1 = 
    let lengths = 
        inputStr.Split [|','|]
        |> List.ofArray
        |> List.map (int)
    let fullArr,_,_ = calc (Array.ofList baseArr) lengths
    fullArr.[0]*fullArr.[1]


let result2 =
    let lengths =
        inputStr
        |> Seq.map int
        |> fun s -> Seq.append s [17; 31; 73; 47; 23]
        |> replicate 64

    let sparseHash,_,_ = calc (Array.ofList baseArr) lengths

    sparseHash
    |> Array.chunkBySize 16
    |> Array.map (Array.reduce (^^^))
    |> Array.map (sprintf "%02x")
    |> String.concat ""