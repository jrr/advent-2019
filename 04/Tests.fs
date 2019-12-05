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
    
[<Fact>]
let ``consecutiveGroups 1`` () =
    consecutiveGroups "a" |> should equal ["a"]
    
[<Fact>]
let ``consecutiveGroups 2`` () =
    consecutiveGroups "aa" |> should equal ["aa"]
    
[<Fact>]
let ``consecutiveGroups 3`` () =
    consecutiveGroups "abbbccb" |> should equal ["b";"cc";"bbb";"a"]


[<Fact>]
let ``matchingNeighborGroupOfLengthTwo false`` () =
    matchingNeighborGroupOfLengthTwo "aaabcccc" |> should equal false
    
[<Fact>]
let ``matchingNeighborGroupOfLengthTwo true`` () =
    matchingNeighborGroupOfLengthTwo "abbbccd" |> should equal true
    
[<Fact>]
let ``matchingNeighborGroupOfLengthTwo examples `` () =
    matchingNeighborGroupOfLengthTwo "112233" |> should equal true
    matchingNeighborGroupOfLengthTwo "123444" |> should equal false
    matchingNeighborGroupOfLengthTwo "111122" |> should equal true
[<Fact>]
let ``solves problem B`` () =
    solve2 "108457-562041" |> should equal 1972
