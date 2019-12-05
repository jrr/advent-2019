module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

let becomes a b = b |> solve |> Seq.map (sprintf "%d") |> (String.concat ",") |> should equal a
    
[<Fact>]
let ``solves example`` () =
    "1,9,10,3,2,3,11,0,99,30,40,50"
        |> becomes "3500,9,10,70,2,3,11,0,99,30,40,50"
        
[<Fact>]
let ``solves other given examples`` () =
            
    
    "1,0,0,0,99" |> becomes "2,0,0,0,99"
    "2,3,0,3,99" |> becomes "2,3,0,6,99"
    "2,4,4,5,99,0" |> becomes "2,4,4,5,99,9801"
    "1,1,1,4,99,5,6,0,99" |> becomes "30,1,1,4,2,5,6,0,99"
            

[<Fact>]
let ``solves problem`` () =
    // before running the program, replace position 1 with the value 12 and replace position 2 with the value 2.
    let input = "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,9,23,1,23,6,27,2,27,13,31,1,10,31,35,1,10,35,39,2,39,6,43,1,43,5,47,2,10,47,51,1,5,51,55,1,55,13,59,1,59,9,63,2,9,63,67,1,6,67,71,1,71,13,75,1,75,10,79,1,5,79,83,1,10,83,87,1,5,87,91,1,91,9,95,2,13,95,99,1,5,99,103,2,103,9,107,1,5,107,111,2,111,9,115,1,115,6,119,2,13,119,123,1,123,5,127,1,127,9,131,1,131,10,135,1,13,135,139,2,9,139,143,1,5,143,147,1,13,147,151,1,151,2,155,1,10,155,0,99,2,14,0,0"

    solve input
        |> Seq.head
        |> should equal 'a'
    // it's not 574684
    
[<Fact>]
let ``instructionAt add`` () =
    instructionAt [1;2;3;4] 0 |> should equal (Add (2,3,4))
    
[<Fact>]
let ``instructionAt multiply`` () =
    instructionAt [2;2;3;4] 0 |> should equal (Multiply (2,3,4))
    
[<Fact>]
let ``instructionAt halt`` () =
    instructionAt [2;2;99;4;5] 2 |> should equal Halt
 
[<Fact>]
let ``replace`` () =
    replace [1;2;3;4;5] 2 99 |> List.ofSeq |> should equal [1;2;99;4;5]


