module Solve

let buildMap (input: string) =
    input.Split('\n')
        |> Seq.ofArray
        |> Seq.map (fun s ->
            let [| a; b |] = s.Split(')')
            (a, b)
            )
        |> Seq.groupBy fst
        |> Seq.map (fun (x, y) -> (x, Seq.map snd y))
        |> Map.ofSeq

type WalkResult = { nodes: int; orbits: int }

let addWalkResults =
    (fun r s -> { nodes = r.nodes + s.nodes; orbits = r.orbits + s.orbits })

let rec walk (map: Map<string, string seq>) (node: string) (depth: int) =
    let children = map |> Map.tryFind node

    let childrenSum = match children with
                        | None -> { nodes = 0; orbits = 0 } // if the node name isn't found, then it is a leaf without children
                        | Some ch ->
                            let results = ch |> Seq.map (fun c -> walk map c (depth + 1))
                            let sum = results |> Seq.fold addWalkResults { nodes = 0; orbits = 0 }
                            sum
    printfn "node %s sum %s" node (childrenSum.ToString())
    addWalkResults childrenSum { nodes = 1; orbits = childrenSum.nodes }

let solve (input: string) =
    let map = buildMap input
    (walk map "COM" 0).orbits

let buildReverseMap (input: string) =
    input.Split('\n')
        |> Seq.ofArray
        |> Seq.map (fun s ->
            let [| a; b |] = s.Split(')')
            (b, a)
            )
        |> Seq.groupBy fst
        |> Seq.map (fun (x, y) -> (x, Seq.map snd y))
        |> Map.ofSeq

(*
new approach:
build tree in reverse
accumulate path from "YOU" to "COM" with depths
accumulate path from "SAN" to "COM" with depths
find intersection of paths
take node with minimum sum depth
*)

let rec getPath (map: Map<string, string seq>) (node:string) =
    let next = map |> Map.find node
    match (next |> List.ofSeq) with
    | [] -> []
    | ["COM"] -> ["COM"]
    | [x] -> node::(getPath map x)
    | _ -> failwith "oops2"
    
let sixBee (input: string) =
    let map = buildReverseMap input
    let path1 = (getPath map "YOU") |> List.mapi (fun i a -> a,i)
    let path2 = (getPath map "SAN") |> List.mapi (fun i a -> a,i)
    (List.append path1 path2
        |> List.groupBy (fun (a,_) -> a)
        |> List.where (fun (_,l) -> l.Length = 2)
        |> List.map (fun (_,l) -> l |> List.map (fun (name,i) -> i))
        |> List.map (fun x -> x |> List.sum)
        |> List.sort
        |> List.head) - 2
