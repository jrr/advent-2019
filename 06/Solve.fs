module Solve
open System
        
let buildMap (input:string) =
    input.Split('\n')
        |> Seq.ofArray
        |> Seq.map (fun s ->
            let [|a;b|] = s.Split(')')
            (a,b)
            )
        |> Seq.groupBy fst
        |> Seq.map (fun (x, y) -> (x, Seq.map snd y))
        |> Map.ofSeq
        
type WalkResult = {nodes:int;orbits:int}

let addWalkResults =
    (fun r s -> {nodes=r.nodes+s.nodes;orbits=r.orbits+s.orbits})
    
let rec walk (map: Map<string,string seq>) (node:string) (depth:int)=
    let children = map |> Map.tryFind node
    
    let childrenSum = match children with
                        | None -> {nodes=0;orbits=0} // if the node name isn't found, then it is a leaf without children
                        | Some ch ->
                            let results = ch |> Seq.map (fun c -> walk map c (depth+1))
                            let sum = results |> Seq.fold addWalkResults {nodes=0; orbits=0}
                            sum
    printfn "node %s sum %s" node (childrenSum.ToString())
    addWalkResults childrenSum {nodes=1;orbits=childrenSum.nodes}
    
let solve (input:string) =
    let map = buildMap input
//    let lookup = map |> Map.find "COM"
//    printfn "map %s" (map.ToString())
    (walk map "COM" 0).orbits
