module Day17

let calcStep steps (list,curr) i =
    let nextI = (curr + steps)%i + 1

    let updated =
        List.take (nextI) list
        @ [i] 
        @ List.skip (nextI) list
    
    (updated, nextI)

let calcAt pos steps (lastFound, curr) i =
    let nextI = (curr + steps)%i + 1
    let valAt = 
        if nextI = pos 
        then i
        else lastFound
    (valAt, nextI)

let result1 =
    let stepFun = calcStep 304

    let (final, lastI) =
        seq[1..2017]
        |> Seq.fold (stepFun) ([0],0)

    final |> List.item (lastI + 1)

let result2 =
    let stepFun = calcAt 1 304

    let (valAt, lastI) =
        seq[2..50_000_000]
        |> Seq.fold (stepFun) (1,1)

    valAt