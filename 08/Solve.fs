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
