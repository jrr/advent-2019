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
type InstructionT = {
    OpCode: int
    Bytes: int
}
type InstructionEnum = Halt = 99 | Add = 1 | Multiply = 2 | Input = 3 | Output = 4
let instructionAt (instructions : int seq) (pos:int) =
    let current = instructions |> Seq.skip pos
    let modeBits,opCode = current |> Seq.head |> decodeOpCode
    let opEnum:InstructionEnum = enum opCode
    let consumeInstructions (n:int) (modeBits:int list) = Seq.skip 1 >> Seq.take n >> Seq.toList >> mapOperands modeBits
    match (opEnum) with
    | InstructionEnum.Halt -> Halt
    | InstructionEnum.Add ->
        let [a;b;c] = current |> consumeInstructions 3 modeBits
        Add (a,b,c)
    | InstructionEnum.Multiply ->
        let [a;b;c] = current |> consumeInstructions 3 modeBits
        Multiply (a,b,c)
    | InstructionEnum.Input ->
        let [a] = current |> consumeInstructions 1 modeBits
        Input a
    | InstructionEnum.Output ->
        let [a] = current |> consumeInstructions 1 modeBits
        Output a
    | _ -> failwith "unimplemented opcode"

