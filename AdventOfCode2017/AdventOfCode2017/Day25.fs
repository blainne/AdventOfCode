module Day25
open System
open System.IO

//This approach with parser combinators is overkill here, 
//but I wanted to practice parser combinators
module Parsing =

    type ParseSuccess<'a> = { Value:'a; Remaining:string }
    type ParseResult<'a> = 
        | Ok of ParseSuccess<'a>
        | Failure

    type Parser<'a> = Parser of (string -> ParseResult<'a>)    

    let runP (Parser p) inp = p inp 
    let parseText (text:string) =
        let innerF (input:string) = 
            if input.StartsWith text
            then Ok({Value=text; Remaining=input.Substring(text.Length)})
            else Failure
        
        Parser innerF

    let bind 
            (f:'a -> Parser<'b>) 
            (p1:Parser<'a>)             
        : Parser<'b> =
        
        let parseFn inp =
            let result1 = runP p1 inp
            match result1 with
            | Ok s -> runP (f (s.Value)) (s.Remaining)                
            | Failure -> Failure

        Parser parseFn

    let (>>=) p f = bind f p

    let retn s = 
        Parser(fun inp -> Ok{Value=s; Remaining=inp})

    let map f p = 
        p >>= (f>>retn)
    
    let apply pf p =
        pf >>= (fun f ->
        p >>= (f>>retn)) 

    let (<*>) = apply

    let lift2 f p1 p2 = 
        retn f <*> p1 <*> p2        

    let (|>>) p f= map f p

    let orElse p1 p2 =
        let parseFn inp =
            match runP p1 inp with
            | Ok s -> Ok s
            | Failure -> runP p2 inp

        Parser(parseFn)

    let (<|>) = orElse

    let andThen p1 p2 =
        p1 >>= (fun result1 -> 
        p2 >>= fun result2 ->
            retn (result1,result2))

    let rec zeroOrMore p =
        (oneOrMore p) <|> (retn [])

    and oneOrMore p = 
        p >>= (fun x ->
        zeroOrMore p >>= fun more ->
            retn (x::more))

    let takeFirst p1 p2 =
        (andThen p1 p2)
            >>= fun (r1,_) -> retn r1

    let takeSecond p1 p2 =
        (andThen p1 p2)
            >>= fun (_,r2) -> retn r2                 
    let (.>>.) = andThen
    let (.>>) = takeFirst
    let (>>.) = takeSecond

    let rec sequence parsers =
        let consP = 
            lift2 (fun h t -> h::t)

        match parsers with
        | [] -> retn []
        | h::t ->  
            consP h (sequence t)

    let rec repeat n p =
        p |> List.replicate n |> sequence

    let parseOneChar = 
        let innerF (inp:string) =
            if inp.Length = 0
            then Failure
            else Ok ({Value = inp.[0]; Remaining = inp.Substring 1 })
            
        Parser innerF

    let parseWord = 
        let innerF (inp:string)= 
            inp.Split([|'\n';' ';'.';':';'-';','|], StringSplitOptions.RemoveEmptyEntries)
            |> fun arr -> Ok{Value = arr.[0]; Remaining = inp.Substring (arr.[0].Length)}

        Parser innerF

    let parseTillEolOrEof =
        let innerF (inp:string) =
            inp.Split '\n'
            |> fun arr -> 
                if Array.length arr = 1
                then Ok{Value = arr.[0]; Remaining = ""}
                else Ok{Value = arr.[0]; Remaining = inp.Substring (arr.[0].Length + 1)}
        
        Parser innerF

open Parsing

let secondLastWordParser prefix=
    prefix
    |> parseText
    >>. parseWord .>> parseTillEolOrEof
 
let secondLastNumParser prefix =
    prefix |> secondLastWordParser |>> int

let initialSetup = 
    secondLastWordParser "Begin in state "
    .>>. secondLastNumParser "Perform a diagnostic checksum after "

type Dir = | Left | Right
type Condition = {valCondition:int; newVal:int; moveDirection:Dir; nextState:string} 

let strToDir = function
    | "left" -> Left
    | "right" -> Right
    | _ -> failwith "invalid direction";

let conditionInstrParser =
    let lineParsers = 
        [       
            (secondLastWordParser "  If the current value is ");
            (secondLastWordParser "    - Write the value ")
            (secondLastWordParser "    - Move one slot to the ");
            (secondLastWordParser "    - Continue with state ");
        ]

    let mapCond (curr::newVal::dir::next::[]) =
        {
            valCondition = int curr; 
            newVal = int newVal;
            moveDirection = strToDir dir;
            nextState = next;
        }

    sequence lineParsers |>> mapCond
let stateConditionsParser = 
    let condParsers = repeat 2 conditionInstrParser
    let stateP = secondLastWordParser "In state "

    parseTillEolOrEof >>. stateP .>>. condParsers

let path = "./AdventOfCode2017/AdventOfCode2017/inputs/Day25-1.txt"
let testpath = "./AdventOfCode2017/AdventOfCode2017/inputs/Day25-test.txt"

let readConfigAndCommands path =
    let parseResult = 
        path 
        |> File.ReadAllText
        |> runP (initialSetup .>>. (oneOrMore stateConditionsParser))   
    
    let initial, stateConditions =
        match parseResult with
        | Failure -> failwith "Couldn't parse the input"
        | Ok r -> r.Value

    (initial, Map.ofList stateConditions)

type MachineState = 
    {memory:Map<int32, int32>; pointer:int32; state:string; }
    with member this.currVal = 
        if this.memory.ContainsKey this.pointer
        then this.memory.[this.pointer]
        else 0

let makeStep machineState (commands:Map<string,Condition list>) =
    let cmd =
        commands.[machineState.state]
        |> List.pick (fun cmd -> 
                if cmd.valCondition = machineState.currVal
                then Some cmd
                else None )
        
    let newMem = Map.add (machineState.pointer) (cmd.newVal) machineState.memory
    let ptrMove = 
        if cmd.moveDirection = Right then 1 else -1

    {memory = newMem; pointer = machineState.pointer + ptrMove; state = cmd.nextState}

let result1 = 
    let (firstState, stepsToGo), commands = 
        readConfigAndCommands path
    
    let machineState = {memory = Map[]; pointer = 0; state = firstState}

    [1..stepsToGo]
    |> List.fold (fun state _ -> makeStep state commands) machineState
    |> (fun ms -> Map.toList ms.memory)
    |> List.filter (fun (_,v) -> v = 1)
    |> List.length