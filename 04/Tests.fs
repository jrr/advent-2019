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
