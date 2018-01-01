module Day25
open System
open System.IO

//This approach is overkill here, 
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
                Ok{Value = arr.[0]; Remaining = inp.Substring (arr.[0].Length + 1)}
        
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



let txt = """Begin in state A.
Perform a diagnostic checksum after 6 steps.
"""

runP initialSetup txt