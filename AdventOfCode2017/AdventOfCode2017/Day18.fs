module Day18
open System.IO

let readRegistry c map =
    if Map.containsKey c map
    then map.[c]
    else 0L 

let update map key fVal  =
    let currVal = readRegistry key map
    map
    |> Map.add key (fVal currVal)

type ValueSource =
| Registry of char
| Const of int64

    member this.GetVal map =
        match this with
        | Const(x) -> x
        | Registry(c) -> readRegistry c map

type Operation =
| Add of char*ValueSource
| Mul of char*ValueSource
| Set of char*ValueSource
| Mod of char*ValueSource
| Jgz of ValueSource*ValueSource
| Rcv of char
| Snd of ValueSource

let (|IsConst|_|) (str:string) = 
   match System.Int64.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

let (|IsRegistry|_|)(str:string) =
   match System.Char.TryParse(str) with
   | (true,c) -> Some(c)
   | _ -> None

let parseVS token = 
    match token with
    | IsConst(c) -> Const(c)
    | IsRegistry(r) -> Registry(r)

let parseOperation (line:string) = 
    let tokens = line.Split [|' '|]
    match tokens.[0] with
    | "add" -> Add(char tokens.[1], parseVS tokens.[2])
    | "set" -> Set(char tokens.[1], parseVS tokens.[2])
    | "mul" -> Mul(char tokens.[1], parseVS tokens.[2])
    | "mod" -> Mod(char tokens.[1], parseVS tokens.[2])
    | "jgz" -> Jgz(parseVS tokens.[1], parseVS tokens.[2])
    | "rcv" -> Rcv(char tokens.[1])
    | "snd" -> Snd(parseVS tokens.[1])

let parseInput lines = 
    lines
    |> Array.map parseOperation

let makeArithmOper map oper =
    match oper with
    | Add(x,y) -> update map x ((+) (y.GetVal map))
    | Mul(x,y) -> update map x ((*) (y.GetVal map))
    | Set(x,y) -> update map x (fun _ -> y.GetVal map)
    | Mod(x,y) -> update map x (fun curr -> curr % (y.GetVal map))


let rec processStep (ops:Operation[]) map i soundPlayed= 
    match ops.[i] with
    | Add(_) | Mul(_) | Set(_) | Mod(_) ->
        processStep ops
            (makeArithmOper map (ops.[i]))
            (i+1) 
            soundPlayed
    | Snd(x) ->
        processStep ops map (i+1) (x.GetVal map)
    | Rcv(x) -> 
        if (map |> readRegistry x) <> 0L
        then soundPlayed
        else processStep ops map (i+1) soundPlayed
    | Jgz(x,y) ->
        let jump = 
            if (x.GetVal map) > 0L
            then y.GetVal map |> int
            else 1
        processStep ops map (i+jump) soundPlayed

let findFirstRcvFreq (ops:Operation[]) =
    processStep ops (Map<char,int64>[]) 0 0L 

type ProgramEffect = | Msg of int64 | Waiting | None | Done
type ProgramState =
    {
        Regs : Map<char,int64>;
        Inbox : int64 list;
        NextI : int;
        Effect : ProgramEffect;
        MsgsSent : int;
    }

let processExecStep (ops:Operation[]) state= 
    let calcNewState() = 
        match ops.[state.NextI] with
        | Add(_) | Mul(_) | Set(_) | Mod(_) ->
            let newRegs =  (makeArithmOper state.Regs (ops.[state.NextI]))
            {state with
                Regs = newRegs; 
                NextI = state.NextI + 1;
                Effect = None }

        | Snd(x) ->
            {state with
                NextI = state.NextI + 1;
                Effect = Msg(x.GetVal (state.Regs));
                MsgsSent = state.MsgsSent + 1 }

        | Rcv(x) -> 
            match state.Inbox with
            | [] -> {state with Effect = Waiting}
            | h::t -> 
                let newRegs =  (makeArithmOper state.Regs (Set(x,Const(h))))    
                {state with
                    Regs = newRegs; 
                    NextI = state.NextI + 1;
                    Effect = None;
                    Inbox = t}

        | Jgz(x,y) ->
            let jump = 
                if (x.GetVal state.Regs) > 0L
                then y.GetVal state.Regs |> int
                else 1

            {state with
                NextI = state.NextI + jump;
                Effect = None }
    
    if Array.length ops <= state.NextI
    then { state with Effect = Done }
    else calcNewState()

let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day18-1.txt"

let testInput = """set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"""
let testInput2 = """snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d"""

let testResult = 
    (testInput.Split [|'\n'|])
    |> parseInput
    |> findFirstRcvFreq 
let result1 = 
    path
    |> File.ReadAllLines
    |> parseInput
    |> findFirstRcvFreq 

let result2 =
    let ops = 
        path
        |> File.ReadAllLines
        |> parseInput 
    
    let state0 = 
        { Regs = Map[('p',0L)]; NextI = 0;
          Effect = None; Inbox = []; MsgsSent = 0}
    
    let state1 = 
        { Regs = Map[('p',1L)]; NextI = 0;
          Effect = None; Inbox = []; MsgsSent = 0}

    let rec runPrograms s0 s1 =
        let recur st0 st1 =
            runPrograms 
                (processExecStep ops st0)
                (processExecStep ops st1)

        match s0.Effect, s1.Effect with
        | Waiting, Waiting -> s0,s1
        | _, Done -> s0,s1
        | Msg(i), Msg(j) ->
            recur
                {s0 with Inbox = s0.Inbox @ [j]}
                {s1 with Inbox = s1.Inbox @ [i]}
        | Msg(i), _ ->
            recur
                s0 
                {s1 with Inbox = s1.Inbox @ [i]}
        | _, Msg(j) ->
            recur
                {s0 with Inbox = s0.Inbox @ [j]}
                s1
        | _,_ -> recur s0 s1
    
    runPrograms state0 state1

