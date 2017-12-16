module Day15

let generator factor = 
    Seq.unfold 
        (fun prev ->
            let elem = (prev*factor)%2147483647L
            Some(elem,elem))
let last16BitMask = 0xffffL
let gen1 = generator 16807L 783L
let gen2 = generator 48271L 325L

let result1 = 
    Seq.zip gen1 gen2
    |> Seq.take 40_000_000
    |> Seq.filter (
        fun (e1, e2) -> 
            (e1 &&& last16BitMask) = (e2 &&& last16BitMask))
    |> Seq.length

let result2 = 
    let gen1f = Seq.filter (fun e -> e%4L = 0L) gen1
    let gen2f = Seq.filter (fun e -> e%8L = 0L) gen2

    Seq.zip gen1f gen2f
    |> Seq.take 5_000_000
    |> Seq.filter (
        fun (e1, e2) -> 
            (e1 &&& last16BitMask) = (e2 &&& last16BitMask))
    |> Seq.length