module Solve
open System

let solve (input:string) =
    let groups = input |> Seq.splitInto 100 // "at most" this many chunks
      
    // assert it actually gave us 100
    assert (groups |> Seq.length = 100)
    
    // and that the groups are evenly sized
    assert (groups |> Seq.map (fun g -> g.Length) |> Seq.distinct  |> Seq.length  = 1)
    
    let groupWithFewestZeroes = groups |> Seq.sortBy (fun g -> g |> Seq.filter (fun c -> c = '0') |> Seq.length) |> Seq.head
    let numberOfOnes = groupWithFewestZeroes |> Seq.filter (fun c -> c = '1') |> Seq.length
    let numberOfTwos = groupWithFewestZeroes |> Seq.filter (fun c -> c = '2') |> Seq.length
    numberOfOnes * numberOfTwos

let addLayer ((a,b):char*char) =
    match (a,b) with
//    | ('0','0') -> '0'
//    | ('1','1') -> '1'
//    | ('2','2') -> '2'
    | (x,'2') -> x // transparent
    | (_,color) -> color
    | _ -> failwith "oops"
    
let fn (a:char[]) (b:char[]) =
    Array.zip a b |> Seq.map addLayer |> Array.ofSeq
let solve8b (input:string) =
    let groups = input |> Seq.splitInto 100 |> Seq.rev // "at most" this many chunks
      
    // assert it actually gave us 100
    assert (groups |> Seq.length = 100)
    
    // and that the groups are evenly sized
    assert (groups |> Seq.map (fun g -> g.Length) |> Seq.distinct  |> Seq.length  = 1)
    
    let firstLayer = groups |> Seq.head
    let remainingLayers = groups |> Seq.skip 1
    let result = remainingLayers |> Seq.fold fn firstLayer  |> System.String
    result
