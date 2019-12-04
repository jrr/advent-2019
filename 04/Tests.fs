module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``My test`` () =
    solve "foo" |> should equal true
