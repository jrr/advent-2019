module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``parse`` () =
    parse "123-456" |> should equal (123,456)
    
[<Fact>]
let ``seqFrom`` () =
    seqFrom (23,25) |> Seq.toList |> should equal [23;24;25]
    
    
[<Fact>]
let ``hasAdjacentMatchingDigits false`` () =
    hasAdjacentMatchingDigits "1234" |> should equal false
    
[<Fact>]
let ``hasAdjacentMatchingDigits true`` () =
    hasAdjacentMatchingDigits "12334" |> should equal true
    
[<Fact>]
let ``monotonicallyIncreasing true`` () =
    monotonicallyIncreasing "12225" |> should equal true
    
[<Fact>]
let ``monotonicallyIncreasing false`` () =
    monotonicallyIncreasing "12315" |> should equal false
    

[<Fact>]
let ``solves problem`` () =
    solve "108457-562041" |> should equal 2779