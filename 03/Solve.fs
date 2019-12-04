module Solve
open System

type Coordinate = { lat: int32; long: int32 }

let expandPathSegment (input:string) =
    let tail = input.Substring 1
    let head = input.[0]
    let n = Int32.Parse tail
    String.replicate n (head.ToString())
    
let expandPath (input:string) =
    input.Split "," |> Seq.map expandPathSegment |> String.concat ""
    
let pointsForPath input =
    "foo"
    
let solve input =
    (*
    approach:
    
    instead of allocating a big buffer, transform each path into a set of coordinates [(x,y);(x,y)].
    
    sort by distance
    
    find first point that exists in both sets
    *)
    
    159
    

