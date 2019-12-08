module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () =
    solve Test_Input.givenInput |> should equal 2176

    

[<Fact>]
let ``solves 8b`` () =
    solve8b Test_Input.givenInput |> should equal ""
    (*
 XX  X   XX  X XXX  X   X
X  X X   XX X  X  X X   X
X     X X XX   XXX   X X 
X      X  X X  X  X   X  
X  X   X  X X  X  X   X  
 XX    X  X  X XXX    X  
    *)
