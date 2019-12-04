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
    
let vectorForDir = function
    | 'U' -> ( 0, 1)
    | 'D' -> ( 0,-1)
    | 'R' -> ( 1, 0)
    | 'L' -> (-1, 0)
    
let addVec a b =
    (fst a + fst b, snd a + snd b)
    
let rec pointsForPathRec (pos:int*int) (input:string) (accum:(int*int) list)=
    if input.Length = 0 then
        accum
    else
        let tail = input.Substring 1
        let head = input.[0]
        let p = (addVec pos (vectorForDir head))
        (pointsForPathRec p tail (p::accum))
        
let pointsForPath input =
    pointsForPathRec (0,0) input [] |> List.rev
    
let manhattan (input:int*int) =
    (input |> fst |> abs) + (input |> snd |> abs)
    
let solve (input:string) =
    let lines = input.Split('\n')
    assert (lines.Length = 2)
    let points1 = lines.[0] |> expandPath |> pointsForPath |> Set.ofSeq
    let points2 = lines.[1] |> expandPath |> pointsForPath |> Set.ofSeq
    let intersection = Set.intersect points1 points2
    intersection |> Set.toSeq |> Seq.sortBy manhattan |> Seq.head |> manhattan
    
    (*
    approach:
    
    instead of allocating a big buffer, transform each path into a set of coordinates [(x,y);(x,y)].
    
    sort by distance
    
    find first point that exists in both sets
    *)
    
    

