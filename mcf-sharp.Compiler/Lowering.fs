




module Lowering

open Ast
open Ir

type LoweringState = { mutable TempCount: int }
let returnVariable = ".return"
let unwrap (node: Node<'a>) =
    node.Value
let listUnwrap (list: Node<'a> list) =
    list |> List.map unwrap

let getFreshTemp (state: LoweringState) =
    let name = $".temp{state.TempCount}"
    state.TempCount <- state.TempCount + 1
    name

let getFreshControlFlowFlagVariable (state: LoweringState) =
    let name = $".condition_flag{state.TempCount}"
    state.TempCount <- state.TempCount + 1
    name

let getArgumentVariable (argumentNumber: int) =
    $".argument{argumentNumber}"

let getControlFlowBlockName (functionName: string, blockType: string, state: LoweringState) =
    let name = $"{functionName}_{blockType}{state.TempCount}"
    state.TempCount <- state.TempCount + 1
    name

let getPlainVariableName (variable: string) =
    "." + variable

// Lowers an expression into a sequence of instructions,
// returning the instructions and the name of the result variable
let rec lowerExpression (state: LoweringState) (expression: Expression) : IrInstruction list * string =
    match expression with
        | IntLiteral n ->
            let temp = getFreshTemp state
            [IrSetConstant (temp, n)], temp
        | StringLiteral s ->
            let temp = getFreshTemp state
            [IrStorageSet (temp, NBTString s)], temp
        | Variable name ->
            [], name

        | BinaryOperator (operator, left, right) ->
            let leftInstructions, leftVariable = lowerExpression state (unwrap left)
            let rightInstructions, rightVariable = lowerExpression state (unwrap right)
            let temp = getFreshTemp state

            let operatorInstruction = 
                match operator with
                    | Add -> IrAdd (temp, rightVariable)
                    | Subtract -> IrSubtract (temp, rightVariable)
                    | Multiply -> IrMultiply (temp, rightVariable)
                    | Divide -> IrMultiply (temp, rightVariable)
                    | operator -> failwith $"Operator type is not valid ({operator})."
            leftInstructions @ rightInstructions @ [IrCopy (temp, leftVariable); operatorInstruction], temp

        | Call (functionName, arguments) ->
            // Lower each argument into a named scoreboard slot, then call
            let argumentInstructions =
                arguments |> 
                    List.mapi (fun i argument -> 
                        let instructions, argumentVariable = lowerExpression state (unwrap argument)
                        let argumentSlot = getArgumentVariable i
                        instructions @ [IrCopy (argumentSlot, argumentVariable)]
                    ) |> List.concat
            argumentInstructions @ [IrCall functionName], returnVariable

// Lowers a comparison expression into an IrConditionalCall
let lowerCondition (state: LoweringState) (condition: Expression) (functionName: string) : IrInstruction list =
    match condition with
        | BinaryOperator (operator, left, right) ->
            let leftInstructions, leftVariable = lowerExpression state (unwrap left)
            let rightInstructions, rightVariable = lowerExpression state (unwrap right)

            let irOperator = 
                match operator with
                    | Equals -> IrEquals
                    | NotEquals -> IrNotEquals
                    | LessThan -> IrLessThan
                    | LessThanOrEqual -> IrLessThanOrEqual
                    | GreaterThan -> IrGreaterThan
                    | GreaterThanOrEqual -> IrGreaterThanOrEqual
                    | operator -> failwith $"Operator type is not valid for conditionals ({operator}). "
            leftInstructions @ rightInstructions @ [IrConditionalCall (leftVariable, irOperator, rightVariable, functionName)]
        | _ -> failwith "Condition must be a comparison expression"

let rec lowerStatement (state: LoweringState) (functionName: string) (statement: Statement) : IrInstruction list * IrFunction list =
    match statement with
        | VariableDeclaration (name, typeHint, Some init) ->
            let initInstructions, initVariable = lowerExpression state (unwrap init)
            initInstructions @ [IrCopy (name, initVariable)], []

        | VariableDeclaration (name, typeHint, None) ->
            match typeHint with
            | Some TypeInt | Some TypeBool -> [IrSetConstant (name, 0)], []
            | _ -> [], []

        | VariableAssignment (name, value) ->
            let valueInstructions, valueVariable = lowerExpression state (unwrap value)
            valueInstructions @ [IrCopy(name, valueVariable)], []

        | Return (Some value) ->
            let valueInstructions, valueVariable = lowerExpression state (unwrap value)
            valueInstructions @ [IrCopy(returnVariable, valueVariable)], []

        | Return (None) ->
            [], []

        | FunctionCall (name, arguments) ->
            let argumentInstructions =
                arguments |> 
                    List.mapi (fun i argument -> 
                        let instructions, argumentVariable = lowerExpression state argument.Value
                        let argumentSlot = getArgumentVariable i
                        instructions @ [IrCopy (argumentSlot, argumentVariable)]
                    ) |> List.concat
            argumentInstructions @ [IrCall name], []

        | If (condition, thenBranch, elseBranch) ->
            // Each branch becomes its own function file
            let controlFlowFlagVariable = getFreshControlFlowFlagVariable state
            let flagSetupInstruction = IrSetConstant (controlFlowFlagVariable, 1)
            let ifFlagSetInstruction = IrSetConstant (controlFlowFlagVariable, 0)

            let thenName = getControlFlowBlockName(functionName, "then", state)
            let thenInstructions, thenFunctions = lowerStatements state functionName (listUnwrap thenBranch)
            let thenFunction = { Name = thenName; Instructions = [ifFlagSetInstruction] @ thenInstructions }
            let ifConditionInstructions = lowerCondition state (unwrap condition) thenName

            let elseFunctions = 
                match elseBranch with
                    | None -> [], []
                    | Some elseStatements ->
                        let elseName = getControlFlowBlockName(functionName, "else", state)
                        let elseInstructions, elseFunctions = lowerStatements state elseName (listUnwrap elseStatements)
                        let elseFunction = { Name = elseName; Instructions = elseInstructions }
                        let flagCondition = lowerCondition state (BinaryOperator(Equals, { Value = Variable(controlFlowFlagVariable); Position = { Line = 0; Column = 0 } }, ({ Value = IntLiteral (1); Position = { Line = 0; Column = 0 } }))) elseName 
                        flagCondition, [elseFunction] @ elseFunctions

            let elseInstructions, elseFunctionList = elseFunctions
            let allInstructions = [flagSetupInstruction] @ ifConditionInstructions @ elseInstructions
            let allFunctions = [thenFunction] @ thenFunctions @ elseFunctionList
            allInstructions, allFunctions

        | While (condition, body) ->
            let whileName = getControlFlowBlockName(functionName, "while", state)
            let bodyInstructions, bodyFunctions = lowerStatements state whileName (listUnwrap body) 

            let conditionFunctionName = getControlFlowBlockName(functionName, "while_condition", state)
            let conditionInstructions = lowerCondition state (unwrap condition) whileName
            let callConditionInstruction = IrCall(conditionFunctionName)

            let whileFunction = { Name = whileName; Instructions = bodyInstructions @ [callConditionInstruction] }
            let conditionFunction = { Name = conditionFunctionName; Instructions = conditionInstructions }

            [callConditionInstruction], [conditionFunction; whileFunction] @ bodyFunctions

        | RawCommand command ->
            [IrRawCommand command], []

and lowerStatements (state: LoweringState) (functionName: string) (statements: Statement list) : IrInstruction list * IrFunction list =
    statements
    |> List.map (lowerStatement state functionName)
    |> List.unzip
    |> fun (instrLists, fnLists) -> List.concat instrLists, List.concat fnLists


let lowerProgram (program: Program) : IrProgram =
    let state = { TempCount = 0 }

    let functions = 
        program.Functions
        |> List.collect (fun func ->
            let instructions, controlFlowFunctions = lowerStatements state func.Name (listUnwrap func.Body) 
            { Name = func.Name; Instructions = instructions } :: controlFlowFunctions
        )

    let taggedFunctions = 
        program.TaggedBlocks
        |> List.collect (fun block ->
            let tagName = match block.Tag with Load -> "load" | Tick -> "tick"
            let instructions, controlFlowFunctions = lowerStatements state tagName (listUnwrap block.Statements) 
            { Name = tagName; Instructions = instructions } :: controlFlowFunctions
        )

    { Functions = functions @ taggedFunctions}