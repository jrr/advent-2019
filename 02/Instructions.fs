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
    | JumpIfTrue of Operand*Operand
    | JumpIfFalse of Operand*Operand
    | LessThan of Operand*Operand*Operand
    | Equals of Operand*Operand*Operand
    
    
let expandModes (modeBits:int list) (num:int) =
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

type InstructionEnum =
    | Halt = 99
    | Add = 1
    | Multiply = 2
    | Input = 3
    | Output = 4
    | JumpIfTrue = 5
    | JumpIfFalse = 6
    | LessThan = 7
    | Equals = 8
    
let instructionAt (instructions : int seq) (pos:int) =
    let currentInstructions = instructions |> Seq.skip pos
    let modeBits,opCode = currentInstructions |> Seq.head |> decodeOpCode
    let opEnum:InstructionEnum = enum opCode
    let consumeOperands (n:int) (modeBits:int list) = Seq.skip 1 >> Seq.take n >> Seq.toList >> mapOperands modeBits
    let consume1Operand () = consumeOperands 1 modeBits >> (fun [a] -> a)
    let consume2Operands () = consumeOperands 2 modeBits >> (fun [a;b] -> (a,b))
    let consume3Operands () = consumeOperands 3 modeBits >> (fun [a;b;c] -> (a,b,c))
    
    match (opEnum) with
    | InstructionEnum.Halt -> Halt
    | InstructionEnum.Add ->
        currentInstructions |> consume3Operands () |> Add
    | InstructionEnum.Multiply ->
        currentInstructions |> consume3Operands () |> Multiply
    | InstructionEnum.Input ->
        currentInstructions |> consume1Operand () |> Input
    | InstructionEnum.Output ->
        currentInstructions |> consume1Operand () |> Output
    | InstructionEnum.JumpIfTrue ->
        currentInstructions |> consume2Operands () |> JumpIfTrue
    | InstructionEnum.JumpIfFalse ->
        currentInstructions |> consume2Operands () |> JumpIfFalse
    | InstructionEnum.LessThan ->
        currentInstructions |> consume3Operands () |> LessThan
    | InstructionEnum.Equals ->
        currentInstructions |> consume3Operands() |> Equals
    | _ -> failwith "unimplemented opcode"

    (*
    Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
    Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
    Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
    Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
    *)
