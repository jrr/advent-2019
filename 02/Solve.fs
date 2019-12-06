module Solve
open System
open System

type Operand = int
type Instruction =
    | Halt
    | Add of Operand*Operand*Operand
    | Multiply of Operand*Operand*Operand
    | Input of Operand
    | Output of Operand
    
let decodeOpCode (input:int) =
    let opCode = input % 100
    let modes = (input / 100)
                    |> sprintf "%d"
                    |> Seq.map string
                    |> Seq.map Int32.Parse
                    |> List.ofSeq
    modes,opCode
    
let instructionAt (instructions : int seq) (pos:int) =
//    let current = instructions |> Seq.skip pos // |> Seq.take 4 |> Seq.toList
    let current = instructions |> Seq.skip pos
    let modes,opCode = current |> Seq.head |> decodeOpCode
    match opCode with
    | 99 -> Halt
    | 1 ->
        let [a;b;c] = (current |> Seq.skip 1 |> Seq.take 3 |> Seq.toList)
        Add (a,b,c)
    | 2 ->
        let _::a::b::c::_  = (current |> Seq.take 4 |> Seq.toList)
        Multiply (a,b,c)
    | 3 ->
        let _::a::_ = (current |> Seq.take 2 |> Seq.toList)
        Input a
    | 4 ->
        let _::a::_ = (current |> Seq.take 2 |> Seq.toList)
        Output (a)
    
        
    
let replace (list:int seq) pos value =
    list |> Seq.mapi (fun i x ->
        if i = pos then
            value
        else
            x
        )
    
let add (instructions : int seq) a b c =
    let num1 = instructions |> Seq.skip a |> Seq.head
    let num2 = instructions |> Seq.skip b |> Seq.head
    let sum = num1 + num2
    replace instructions c sum
    
let multiply (instructions : int seq) a b c =
    let num1 = instructions |> Seq.skip a |> Seq.head
    let num2 = instructions |> Seq.skip b |> Seq.head
    let sum = num1 * num2
    replace instructions c sum
    
let rec process (instructions : int seq) (pos:int) =
    let instruction = instructionAt instructions pos
    
    match instruction with
    | Halt ->
        instructions
    | Add (a,b,c) ->
        let updated = add instructions a b c
        process updated (pos+4)
    | Multiply (a,b,c) ->
        let updated = multiply instructions a b c
        process updated (pos+4)
    
let solve (input:string) =
    let instructions = input.Split ',' |> Seq.map Int32.Parse
    process instructions 0
    
let solveWithParams (input:string) a b =
    let instructions = input.Split ',' |> Seq.map Int32.Parse
    let instructions = replace instructions 1 a
    let instructions = replace instructions 2 b
    
    process instructions 0
let paramSeq () =
    seq {
        for i in [0..99] do
            for j in [0..99] do
                yield (i,j)
    }
    
let tryParams (input:string) =
    paramSeq ()
        |> Seq.tryFind (fun (i,j) -> solveWithParams input i j |> Seq.head = 19690720 )
