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
    solve8b Test_Input.givenInput |> should equal "011001000110010111001000110010100011010010010100011000001010110001110001010100000010010100100100010010010001001010010010001000110000100100101110000100"
    
    (*
 XX  X   XX  X XXX  X   X
X  X X   XX X  X  X X   X
X     X X XX   XXX   X X 
X      X  X X  X  X   X  
X  X   X  X X  X  X   X  
 XX    X  X  X XXX    X  
    *)
