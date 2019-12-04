module Solve
open System

let parse (input:string) =
    input.Split '-' |> List.ofArray |> List.map Int32.Parse |> fun [a;b] -> a,b
    
let seqFrom ((a,b):int*int) =
    seq {
        for i in [a..b] do
            yield i
    }
    
let solve (input:string) =
    true

