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
    
    (*
    approach:
    
    instead of allocating a big buffer, transform each path into a set of coordinates [(x,y);(x,y)].
    
    sort by distance
    
    find first point that exists in both sets
    *)

    let lines = input.Split('\n')
    assert (lines.Length = 2)
    let points1 = lines.[0] |> expandPath |> pointsForPath |> Set.ofSeq
    let points2 = lines.[1] |> expandPath |> pointsForPath |> Set.ofSeq
    let intersection = Set.intersect points1 points2
    intersection |> Set.toSeq |> Seq.sortBy manhattan |> Seq.head |> manhattan
    
let countingNumbers () = Seq.initInfinite id |> Seq.skip 1

type WireCoord = { wireLen:int; pos:int*int;which:char;}
let processGroup (g:WireCoord seq) =
    let firstWirePoints = g |> Seq.where (fun w -> w.which = 'A')
    let secondWirePoints = g |> Seq.where (fun w -> w.which = 'B')
    if firstWirePoints |> Seq.length = 0 then
        None
    else if secondWirePoints |> Seq.length = 0 then
        None
    else
        let shortestA = firstWirePoints |> Seq.map (fun w -> w.wireLen) |> Seq.sort |> Seq.head
        let shortestB = secondWirePoints |> Seq.map (fun w -> w.wireLen) |> Seq.sort |> Seq.head
//        sprintf "group len %d" (g |> Seq.length) |> Some
        shortestA + shortestB |> Some
let solve2 (input:string) =
    let lines = input.Split('\n')
    assert (lines.Length = 2)
//    let counting = Seq.initInfinite id |> Seq.skip 1

    let points1 = lines.[0] |> expandPath |> pointsForPath |> Seq.mapi (fun i s -> {wireLen=i;pos=s;which='A'})
    let points2 = lines.[1] |> expandPath |> pointsForPath |> Seq.mapi  (fun i s -> {wireLen=i;pos=s;which='B'})
    let allPoints = Seq.append points1 points2
    let groups = allPoints |> Seq.groupBy (fun c -> c.pos) |> Seq.map snd
    
    (groups |> Seq.map processGroup |> List.ofSeq |> List.choose id |> List.sort |> List.head) + 2
    
