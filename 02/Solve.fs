module Solve
open System

type Operand =
    | Immediate of int
    | Position of int

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
    
type OperandMode =
    | PositionMode
    | ImmediateMode
    
let expandModes (modeBits:int list) (num:int) =
//    Seq.replicate 0
    let leadingZeroes = List.replicate ( num - modeBits.Length ) 0
    let altogether = Seq.append leadingZeroes modeBits
    altogether |> Seq.map (function
                            | 0 -> PositionMode
                            | 1 -> ImmediateMode
                            | _ -> failwith "oops" )
    
let mapOperands (modeBits:int list) (operands:int list) : Operand list =
    let expandedModes = expandModes modeBits operands.Length
    let zipped = Seq.zip operands expandedModes
    zipped |> Seq.map (fun (operand,mode) ->
            match mode with
            | PositionMode -> Position operand
            | ImmediateMode -> Immediate operand
        ) |> List.ofSeq
    
let instructionAt (instructions : int seq) (pos:int) =
    let current = instructions |> Seq.skip pos
    let modeBits,opCode = current |> Seq.head |> decodeOpCode
    match opCode with
    | 99 -> Halt
    | 1 ->
        let numOperands = 3
        let [a;b;c] = (current |> Seq.skip 1 |> Seq.take numOperands |> Seq.toList) |> mapOperands modeBits
        Add (a,b,c)
    | 2 ->
        let _::a::b::c::_  = (current |> Seq.take 4 |> Seq.toList)|> mapOperands modeBits
        Multiply (a,b,c)
    | 3 ->
        let _::a::_ = (current |> Seq.take 2 |> Seq.toList)|> mapOperands modeBits
        Input a
    | 4 ->
        let _::a::_ = (current |> Seq.take 2 |> Seq.toList)|> mapOperands modeBits
        Output (a)
    
        
    
let replace (list:int seq) pos value =
    list |> Seq.mapi (fun i x ->
        if i = pos then
            value
        else
            x
        )
    
let lookupPosition instructions pos =
    instructions |> Seq.skip pos |> Seq.head
    
let lookupOperand instructions op =
    match op with
    | Immediate x -> x
    | Position x -> lookupPosition instructions x
    
let add (instructions : int seq) a b c =
    let num1 = a |> lookupOperand instructions
    let num2 = b |> lookupOperand instructions

    let sum = num1 + num2
    
    let destination = match c with
                        | Immediate _ -> failwith "oops"
                        | Position x -> x
                        
    replace instructions destination sum
    
let multiply (instructions : int seq) a b c =
    let num1 = a |> lookupOperand instructions
    let num2 = b |> lookupOperand instructions
    let product = num1 * num2
    let destination = match c with
                        | Immediate _ -> failwith "oops"
                        | Position x -> x
    replace instructions destination product
    
let input  (instructions : int seq) a inputValue =
    let destination = match a with
                        | Immediate _ -> failwith "oops"
                        | Position x -> x
    replace instructions destination inputValue
    
type IOFunctions = {
        InputFunction : unit -> int;
        OutputFunction : int -> unit;
    }

let rec process (instructions : int seq) (pos:int) (io:IOFunctions) =
    let instruction = instructionAt instructions pos
    
    match instruction with
    | Halt ->
        instructions
    | Add (a,b,c) ->
        let updated = add instructions a b c
        process updated (pos+4) io
    | Multiply (a,b,c) ->
        let updated = multiply instructions a b c
        process updated (pos+4) io
    | Input a ->
        let inputValue = io.InputFunction ()
        let updated = input instructions a inputValue
        process updated (pos+2) io
    | Output a ->
        let outputValue = a |> lookupOperand instructions
        io.OutputFunction outputValue
        process instructions (pos+2) io
    | _ -> failwith "unimplemented instruction"
    
let nopIo : IOFunctions = {
    InputFunction = fun () -> 5 ; //failwith "oops";
    OutputFunction = fun _ -> ()
}
let solve (input:string) =
    let instructions = input.Split ',' |> Seq.map Int32.Parse
    process instructions 0 nopIo
    
let solveWithParams (input:string) a b =
    let instructions = input.Split ',' |> Seq.map Int32.Parse
    let instructions = replace instructions 1 a
    let instructions = replace instructions 2 b
    
    process instructions 0 nopIo
    
let paramSeq () =
    seq {
        for i in [0..99] do
            for j in [0..99] do
                yield (i,j)
    }
    
let tryParams (input:string) =
    paramSeq ()
        |> Seq.tryFind (fun (i,j) -> solveWithParams input i j |> Seq.head = 19690720 )
