module Tests

open System
open System.Xml.Linq
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves example`` () =
    solve Test_Inputs.puzzle6example |> should equal 42
    
[<Fact>]
let ``solves 6A`` () =
    solve Test_Inputs.puzzle6input |> should equal 253104
    
[<Fact>]
let ``walk single node`` () =
    let map = Map.empty.Add ("COM",Seq.empty)
    walk map "COM" 0 |> should equal {nodes=1; orbits=0}
    
[<Fact>]
let ``walk two-node`` () =
    (*    COM -- A -- B    *)
    let input = "COM)A"
    let map = buildMap input
    walk map "COM" 0 |> should equal {nodes=2; orbits=1}
    
[<Fact>]
let ``walk three-node linear`` () =
    (*    COM -- A -- B    *)
    let input = "COM)A
A)B"
    let map = buildMap input
    walk map "COM" 0 |> should equal {nodes=3; orbits=3}
    
[<Fact>]
let ``walk three-node fork`` () =
    (*    COM -- A
              \
                B     *)
    let input = "COM)A
COM)B"
    let map = buildMap input
    walk map "COM" 0 |> should equal {nodes=3; orbits=2}
    
[<Fact>]
let ``walk four-node linear`` () =
    (*    COM -- A -- B -- C   *)
    let input = "COM)A
A)B
B)C"
    let map = buildMap input
    walk map "COM" 0 |> should equal {nodes=4; orbits=6}
    
[<Fact>]
let ``walk four-node fork`` () =
    (*    COM -- A -- B
                   \
                     C      *)
    let input = "COM)A
A)B
A)C"
    let map = buildMap input
    walk map "COM" 0 |> should equal {nodes=4; orbits=5}
