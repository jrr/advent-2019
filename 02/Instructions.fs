module Instructions
open System

type OperandMode =
    | PositionMode
    | ImmediateMode
    
type Operand =
    | Immediate of int
    | Position of int
    
type Instruction =
    | Halt
    | Add of Operand*Operand*Operand
    | Multiply of Operand*Operand*Operand
    | Input of Operand
    | Output of Operand
    
let expandModes (modeBits:int list) (num:int) =
//    Seq.replicate 0
    let leadingZeroes = List.replicate ( num - modeBits.Length ) 0
    let altogether = Seq.append leadingZeroes modeBits
    altogether |> Seq.map (function
                            | 0 -> PositionMode
                            | 1 -> ImmediateMode
                            | _ -> failwith "oops" )
let mapOperands (modeBits:int list) (operands:int list) : Operand list =
    let expandedModes = expandModes modeBits operands.Length |> Seq.rev
    let zipped = Seq.zip operands expandedModes
    zipped |> Seq.map (fun (operand,mode) ->
            match mode with
            | PositionMode -> Position operand
            | ImmediateMode -> Immediate operand
        ) |> List.ofSeq
    
    
    
let decodeOpCode (input:int) =
    let opCode = input % 100
    let modes = (input / 100)
                    |> sprintf "%d"
                    |> Seq.map string
                    |> Seq.map Int32.Parse
                    |> List.ofSeq
    modes,opCode
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
        let numOperands = 3
        let [a;b;c] = (current |> Seq.skip 1 |> Seq.take numOperands |> Seq.toList)|> mapOperands modeBits
        Multiply (a,b,c)
    | 3 ->
        let numOperands = 1
        let [a] = (current |> Seq.skip 1 |> Seq.take numOperands |> Seq.toList)|> mapOperands modeBits
        Input a
    | 4 ->
        let numOperands = 1
        let [a] = (current |> Seq.skip 1 |> Seq.take numOperands |> Seq.toList)|> mapOperands modeBits
        Output a
    | _ -> failwith "unimplemented opcode"

