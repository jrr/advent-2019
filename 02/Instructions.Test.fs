module InstructionTests

open System
open Xunit
open FsUnit.Xunit
//open Solve
open Instructions

    

    
[<Fact>]
let ``instructionAt add`` () =
    instructionAt [1;2;3;4] 0 |> should equal (Add (Position 2,Position 3,Position 4))
    
[<Fact>]
let ``instructionAt multiply`` () =
    instructionAt [2;2;3;4] 0 |> should equal (Multiply (Position 2,Position 3,Position 4))
    
[<Fact>]
let ``instructionAt halt`` () =
    instructionAt [2;2;99;4;5] 2 |> should equal Halt
    
[<Fact>]
let ``instructionAt input`` () =
    instructionAt [3;50;2;2;3;4] 0 |> should equal (Input (Position 50))
    
[<Fact>]
let ``instructionAt output`` () =
    instructionAt [4;23;2] 0 |> should equal (Output (Position 23))
 
[<Fact>]
let ``decodeOpCode simple`` () =
    decodeOpCode 3 |> should equal ([0],3)
    
[<Fact>]
let ``decodeOpCode with modes`` () =
    decodeOpCode 10123 |> should equal ([1;0;1],23)
    
[<Fact>]
let ``expandModes`` () =
    expandModes [1;0;1] 5 |> List.ofSeq |> should equal [PositionMode;PositionMode;ImmediateMode;PositionMode;ImmediateMode]
    
    

