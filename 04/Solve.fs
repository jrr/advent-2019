module Solve
open System

let parse (input:string) =
    input.Split '-' |> List.ofArray |> List.map Int32.Parse |> fun [a;b] -> a,b
    
let seqFrom ((a,b):int*int) =
    seq {
        for i in [a..b] do
            yield i
    }
    
    (*
    The value is within the range given in your puzzle input.
    It is a six-digit number.
    Two adjacent digits are the same (like 22 in 122345).
    Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
    *)
    
let lengthSix s = (Seq.length s) = 6



let hasAdjacentMatchingDigits (s:string) =
    s |> Seq.pairwise |> Seq.tryFind (fun (a,b) -> a = b) |> Option.isSome

let monotonicallyIncreasing (s:string) =
    s |> Seq.pairwise |> Seq.tryFind (fun (a,b) -> a > b) |> Option.isNone

let consecutiveGroups (input:string) : string list =
    input |> Seq.fold (fun (accum:string list) (b:char) ->
        match accum,b with
        | [],b -> [sprintf "%c" b]
        | head::tail,b ->
            if head.[0] = b then
                (sprintf "%s%c" head b)::tail
            else
                (sprintf "%c" b)::head::tail
        
//        m
//        let head = accum |> List.head
//        let tail = accum |> List.tail
//        printfn "head tail? %s %s" (head.ToString()) (tail.ToString())
//        if accum.Length = 0 then
//            [sprintf "%c" b]
//        else
//            let head::tail = accum
//    ////        printfn "asdf"
//    //        []
//            if head.Length = 0 then
//                [sprintf "%c" b]
//            else
//                let result = (sprintf "%s%c" head b)::tail
//                result
            
        ) []
//    [input]
    
    
let matchingNeighborGroupOfLengthTwo (s:string) =
    s |> consecutiveGroups |> Seq.tryFind (fun g -> g.Length = 2) |> Option.isSome
    
let solve (input:string) =
    input
        |> parse
        |> seqFrom
        |> Seq.map (fun s -> s.ToString())
        |> Seq.where lengthSix
        |> Seq.where hasAdjacentMatchingDigits
        |> Seq.where monotonicallyIncreasing
        |> Seq.length
   


let solve2 (input:string) =
    input
        |> parse
        |> seqFrom
        |> Seq.map (fun s -> s.ToString())
        |> Seq.where lengthSix
        |> Seq.where matchingNeighborGroupOfLengthTwo
        |> Seq.where monotonicallyIncreasing
        |> Seq.length
