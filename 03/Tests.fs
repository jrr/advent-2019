module Tests

open System
open Xunit
open FsUnit.Xunit

let foo x =
    x + 5

[<Fact>]
let ``My test`` () =
    true |> should equal true
    foo 5 |> should equal 10
