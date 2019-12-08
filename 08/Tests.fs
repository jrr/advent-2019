module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () =
    solve Test_Input.givenInput |> should equal 2176
    

