module Solve
open System
open Instructions
open InstructionImplementations
    
let replace (list:int seq) pos value =
    list |> Seq.mapi (fun i x ->
        if i = pos then
            value
        else
            x
        )
    
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
        let updatedInstructions,iptr = add instructions a b c pos
        process updatedInstructions iptr io
    | Multiply (a,b,c) ->
        let updatedInstructions,iptr = multiply instructions a b c pos
        process updatedInstructions iptr io
    | Input a ->
        let inputValue = io.InputFunction ()
        let updatedInstructions,iptr = input instructions a inputValue pos
        process updatedInstructions iptr io
    | Output a ->
        let updatedInstructions,iptr,outputValue = output instructions a pos
        io.OutputFunction outputValue
        process updatedInstructions iptr io
    | JumpIfTrue (a,b) ->
        let updatedInstructions,iptr = jumpIfTrue instructions a b pos
        process updatedInstructions iptr io
    | JumpIfFalse (a,b) ->
        let updatedInstructions,iptr = jumpIfFalse instructions a b pos
        process updatedInstructions iptr io
    | LessThan (a,b,c) ->
        let updatedInstructions,iptr = lessThan instructions a b c pos
        process updatedInstructions iptr io
    | Equals (a,b,c) ->
        let updatedInstructions,iptr = equals instructions a b c pos
        process updatedInstructions iptr io
        
    
    (*
    Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
    Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
    Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
    Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
    *)
    | i -> failwith (sprintf "unimplemented instruction %s" <| i.ToString())
    
let nopIo : IOFunctions = {
    InputFunction = fun () -> 5 ; //failwith "oops";
    OutputFunction = fun _ -> ()
}

let solveWithIO (input:string) (io:IOFunctions) =
    let instructions = input.Split ',' |> Seq.map Int32.Parse
    process instructions 0 io
    
let solve (input:string) = solveWithIO input nopIo
    
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
