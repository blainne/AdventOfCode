module Day16

type Instr =
    | Spin of int
    | Exchange of int*int
    | Partner of char*char

let parseInstruction (str:string) =
    match str.[0] with
    | 's' -> Spin(str.Substring(1,1) |> int)
    | 'x' -> 
        let tokens = str.Substring(1).Split([|'/'|])
        Exchange(int tokens.[0], int tokens.[1])
    | 'p' -> Partner(str.[1],str.[3])


let parseInput (str:string) =
    str.Split [|','|]
    |> Array.map parseInstruction
    |> Array.toList


let testInstructions = "s1,x3/4,pe/b,x15/13"
parseInput testInstructions