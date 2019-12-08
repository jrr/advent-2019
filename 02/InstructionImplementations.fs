module InstructionImplementations
open Instructions
    
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
    
let add (instructions : int seq) a b c (iptr:int)=
    let num1 = a |> lookupOperand instructions
    let num2 = b |> lookupOperand instructions

    let sum = num1 + num2
    
    let destination = match c with
                        | Immediate _ -> failwith "oops"
                        | Position x -> x
                        
    (replace instructions destination sum),iptr+4
    
let multiply (instructions : int seq) a b c (pos:int)=
    let num1 = a |> lookupOperand instructions
    let num2 = b |> lookupOperand instructions
    let product = num1 * num2
    let destination = match c with
                        | Immediate _ -> failwith "oops"
                        | Position x -> x
    (replace instructions destination product),pos+4
    
let input (instructions : int seq) a inputValue (pos:int)=
    let destination = match a with
                        | Immediate _ -> failwith "oops"
                        | Position x -> x
    (replace instructions destination inputValue),pos+2
    
let output (instructions : int seq) a (pos:int)=
    let outputValue = a |> lookupOperand instructions
    instructions,(pos+2),outputValue
    
let jumpIfTrue (instructions : int seq) a b (pos:int)=
    let num1 = a |> lookupOperand instructions
    let num2 = b |> lookupOperand instructions
    if num1 = 0 then
        instructions,(pos+3)
    else
        instructions,(num2)
            
let jumpIfFalse (instructions : int seq) a b (pos:int)=
    let num1 = a |> lookupOperand instructions
    let num2 = b |> lookupOperand instructions
    if num1 <> 0 then
        instructions,(pos+3)
    else
        instructions,(num2)
        
let equals (instructions : int seq) a b c (pos:int)=
    let num1 = a |> lookupOperand instructions
    let num2 = b |> lookupOperand instructions
    
    let destination = match c with
                        | Immediate _ -> failwith "oops"
                        | Position x -> x
    if num1 = num2 then
        (replace instructions destination 1),pos+4
    else
        (replace instructions destination 0),pos+4
        
let lessThan (instructions : int seq) a b c (pos:int)=
    let num1 = a |> lookupOperand instructions
    let num2 = b |> lookupOperand instructions
    
    let destination = match c with
                        | Immediate _ -> failwith "oops"
                        | Position x -> x
    if num1 < num2 then
        (replace instructions destination 1),pos+4
    else
        (replace instructions destination 0),pos+4
        
