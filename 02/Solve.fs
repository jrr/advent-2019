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
    | _ -> failwith "unimplemented instruction"
    
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
