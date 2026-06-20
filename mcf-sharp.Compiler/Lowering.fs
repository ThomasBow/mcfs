




module Lowering

open Ast
open Ir

type LoweringState = { mutable TempCount: int }
let returnVariable = ".return"


let getFreshTemp (state: LoweringState) =
    let name = $".temp{state.TempCount}"
    state.TempCount <- state.TempCount + 1
    name

let getFreshControlFlowFlagVariable (state: LoweringState) =
    let name = $".conditionFlag{state.TempCount}"
    state.TempCount <- state.TempCount + 1
    name

let getArgumentVariable (argumentNumber: int) =
    $".argument{argumentNumber}"

let getControlFlowBlockName (functionName: string, blockType: string, state: LoweringState) =
    let name = $"{functionName}_{blockType}{state.TempCount}"
    state.TempCount <- state.TempCount + 1
    name

// Lowers an expression into a sequence of instructions,
// returning the instructions and the name of the result variable
let rec lowerExpression (state: LoweringState) (expression: Expression) : IrInstruction list * string =
    match expression with
        | IntLiteral n ->
            let temp = getFreshTemp state
            [IrSetConstant (temp, n)], temp

        | Variable name ->
            [], name

        | BinaryOperator (operator, left, right) ->
            let leftInstructions, leftVariable = lowerExpression state left
            let rightInstructions, rightVariable = lowerExpression state right
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
                        let instructions, argumentVariable = lowerExpression state argument
                        let argumentSlot = getArgumentVariable i
                        instructions @ [IrCopy (argumentSlot, argumentVariable)]
                    ) |> List.concat
            argumentInstructions @ [IrCall functionName], returnVariable

// Lowers a comparison expression into an IrConditionalCall
let lowerCondition (state: LoweringState) (condition: Expression) (functionName: string) : IrInstruction list =
    match condition with
        | BinaryOperator (operator, left, right) ->
            let leftInstructions, leftVariable = lowerExpression state left
            let rightInstructions, rightVariable = lowerExpression state right

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
        | VariableDeclaration (name, Some init) ->
            let initInstructions, initVariable = lowerExpression state init
            initInstructions @ [IrCopy (name, initVariable)], []

        | VariableDeclaration (_, None) ->
            [], []

        | VariableAssignment (name, value) ->
            let valueInstructions, valueVariable = lowerExpression state value
            valueInstructions @ [IrCopy(name, valueVariable)], []

        | Return (Some value) ->
            let valueInstructions, valueVariable = lowerExpression state value
            valueInstructions @ [IrCopy(returnVariable, valueVariable)], []

        | Return (None) ->
            [], []

        | FunctionCall (name, arguments) ->
            let argumentInstructions =
                arguments |> 
                    List.mapi (fun i argument -> 
                        let instructions, argumentVariable = lowerExpression state argument
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
            let thenInstructions, thenFunctions = lowerStatements state functionName thenBranch
            let thenFunction = { Name = thenName; Instructions = [ifFlagSetInstruction] @ thenInstructions }
            let ifConditionInstructions = lowerCondition state condition thenName

            let elseFunctions = 
                match elseBranch with
                    | None -> [], []
                    | Some elseStatements ->
                        let elseName = getControlFlowBlockName(functionName, "else", state)
                        let elseInstructions, elseFunctions = lowerStatements state elseName elseStatements
                        let elseFunction = { Name = elseName; Instructions = elseInstructions }
                        let flagCondition = lowerCondition state (BinaryOperator(Equals, Variable(controlFlowFlagVariable), (IntLiteral (1)))) elseName 
                        flagCondition, [elseFunction] @ elseFunctions

            let elseInstructions, elseFunctionList = elseFunctions
            let allInstructions = [flagSetupInstruction] @ ifConditionInstructions @ elseInstructions
            let allFunctions = [thenFunction] @ thenFunctions @ elseFunctionList
            allInstructions, allFunctions

        | While (condition, body) ->
            let whileName = getControlFlowBlockName(functionName, "while", state)
            let bodyInstructions, bodyFunctions = lowerStatements state whileName body

            let conditionFunctionName = getControlFlowBlockName(functionName, "while_condition", state)
            let conditionInstructions = lowerCondition state condition whileName
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
            let instructions, controlFlowFunctions = lowerStatements state func.Name func.Body
            { Name = func.Name; Instructions = instructions } :: controlFlowFunctions
        )

    let taggedFunctions = 
        program.TaggedBlocks
        |> List.collect (fun block ->
            let tagName = match block.Tag with Load -> "load" | Tick -> "tick"
            let instructions, controlFlowFunctions = lowerStatements state tagName block.Statements
            { Name = tagName; Instructions = instructions } :: controlFlowFunctions
        )

    { Functions = functions @ taggedFunctions}