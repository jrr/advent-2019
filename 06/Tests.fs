module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () =
    solve "foo" |> should equal "foo"
    
