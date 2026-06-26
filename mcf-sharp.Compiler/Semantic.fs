



module Semantic

open Ast

let operatorSymbol operator =
    match operator with
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Equals -> "=="
    | NotEquals -> "!="
    | LessThan -> "<"
    | GreaterThan -> ">"
    | LessThanOrEqual -> "<="
    | GreaterThanOrEqual -> ">="

type SemanticError =
    | UndeclaredVariable of string
    | InvalidVariableDeclaration of string
    | UndeclaredFunction of string
    | ReturnOutsideFunction of string
    | TypeMismatch of string
    | IncorrectArgumentAmount of string

let analyse (program: Program) : Result<Program, SemanticError list> =    

    let errors = System.Collections.Generic.List<SemanticError>()
    let declaredFunctions = program.Functions |> List.map (fun func -> func.Name, func) |> Map.ofList

    let rec checkExpression (declaredVariables: Map<string, Type>)(expression: Node<Expression>) : Type =
        match expression with
            | { Value = IntLiteral _ } -> TypeInt
            | { Value = StringLiteral _ } -> TypeString
            | { Value = Variable name; Position = pos } ->
                match Map.tryFind name declaredVariables with
                    | Some t -> t
                    | None -> 
                        errors.Add (UndeclaredVariable $"Undeclared Variable {name} (Location {pos.Line}:{pos.Column})")
                        TypeError
            | { Value = BinaryOperator (operator, left, right); Position = pos } ->
                let leftType = checkExpression declaredVariables left
                let rightType = checkExpression declaredVariables right
                match operator with
                    | Add | Subtract | Multiply | Divide ->
                        match (leftType, rightType) with 
                            | (TypeInt, TypeInt) -> TypeInt
                            | _ -> 
                                errors.Add (TypeMismatch $"Operator type mismatch ({leftType}{operator}{rightType}) (Location {pos.Line}:{pos.Column})")
                                TypeError
                    | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual ->
                        match (leftType, rightType) with 
                            | (TypeInt, TypeInt) -> TypeBool
                            | _ -> 
                                errors.Add (TypeMismatch $"Operator type mismatch ({leftType}{operator}{rightType}) (Location {pos.Line}:{pos.Column})")
                                TypeError
                    | Equals | NotEquals ->
                        match (leftType, rightType) with 
                            | (TypeInt, TypeInt) 
                            | (TypeBool, TypeBool)
                            | (TypeString, TypeString) -> TypeBool
                            | _ -> 
                                errors.Add (TypeMismatch $"Operator type mismatch ({leftType}{operatorSymbol operator}{rightType}) (Location {pos.Line}:{pos.Column})")
                                TypeError
                        
            | { Value = Call(name, arguments); Position = pos } ->               
                match Map.tryFind name declaredFunctions with
                    | None ->
                        errors.Add (UndeclaredFunction $"Undeclared function '{name}' (Location {pos.Line}:{pos.Column})")
                        TypeError
                    | Some func ->
                        if List.length arguments <> List.length func.Parameters then
                            errors.Add (IncorrectArgumentAmount $"{func.Name} expects {List.length func.Parameters} arguments, but got {List.length arguments} (Location {pos.Line}:{pos.Column})")
                            TypeError
                        else
                            List.zip arguments func.Parameters
                            |> List.iter (fun (argument, parameter) ->
                                checkExpressionExpectType declaredVariables parameter.Type argument |> ignore
                            )
                            func.ReturnType
        

    and checkExpressionExpectType (declaredVariables: Map<string, Type>) (expectedType: Type) (expression: Node<Expression>) : Type =

        let expressionType = checkExpression declaredVariables expression
        if expectedType <> expressionType then 
            if expressionType <> TypeError then
                errors.Add (TypeMismatch $"Expression expected type {expectedType}, but got {expressionType} (Location {expression.Position.Line}:{expression.Position.Column})" )
            TypeError
        else expectedType

    and checkExpressionTypeOption (declaredVariables: Map<string, Type>) (expectedType: Type option) (expression: Node<Expression>) : Type =
        match expectedType with
        | Some t -> checkExpressionExpectType declaredVariables t expression
        | None -> checkExpression declaredVariables expression
                        
    
    let rec checkStatement (declared: Map<string, Type>) (returnType: Type) (statement: Node<Statement>) : Map<string, Type> =
        match statement with
            | { Value = VariableDeclaration(name, typeHint, initializer); Position = pos } ->
                let declarationType =
                    match initializer with
                        | Some init -> 
                            checkExpressionTypeOption declared typeHint init
                        | None -> 
                            match typeHint with
                                | Some t ->
                                    t
                                | None -> 
                                    errors.Add (InvalidVariableDeclaration $"Variables without initializers must be given a type. ({name}) (Location {pos.Line}:{pos.Column})")
                                    TypeError
                Map.add name declarationType declared
                
            | { Value = VariableAssignment(name, value); Position = pos } ->
                match Map.tryFind name declared with
                    | Some variableType ->
                        checkExpressionExpectType declared variableType value |> ignore
                    | None -> 
                        errors.Add (UndeclaredVariable $"Undeclared Variable {name} (Location {pos.Line}:{pos.Column})")  
                declared
            | { Value = If(condition, thenBranch, elseBranch) } ->
                checkExpressionExpectType declared TypeBool condition |> ignore
                List.fold (fun accu statement -> checkStatement accu returnType statement) declared thenBranch |> ignore 
                elseBranch |> Option.iter (
                    fun statements -> List.fold (fun accu statement -> checkStatement accu returnType statement) declared statements |> ignore
                )
                declared
            | { Value = While(condition, body) } ->
                checkExpressionExpectType declared TypeBool condition |> ignore
                List.fold (fun accu statement -> checkStatement accu returnType statement) declared body |> ignore
                declared
            | { Value = FunctionCall(name, arguments); Position = pos } ->
                match Map.tryFind name declaredFunctions with 
                    | None -> 
                        errors.Add (UndeclaredFunction $"Undeclared Function {name} (Location {pos.Line}:{pos.Column})")
                    | _ -> ()                    
                List.iter (checkExpression declared >> ignore) arguments
                declared
            | { Value = Return value } ->
                value |> Option.iter (fun expression -> checkExpressionExpectType declared returnType expression |> ignore) 
                declared
            | { Value = RawCommand _ } -> 
                declared

    // Gather all variables declared in @load blocks
    let loadBlockDeclaredVariables =
        program.TaggedBlocks
        |> List.filter (fun block -> block.Tag = Load)
        |> List.collect (fun block -> block.Statements)
        |> List.fold (fun accu statement -> checkStatement accu TypeError statement) Map.empty            

    let checkFunction (func: FunctionDefinition) =
        let declared = 
            func.Parameters 
            |> List.fold (fun accu parameter -> Map.add parameter.Name parameter.Type accu) loadBlockDeclaredVariables
        List.fold (fun accu statement -> checkStatement accu func.ReturnType statement) declared func.Body |> ignore

    // Also check the tagged blocks
    let checkTaggedBlock (block: TaggedBlock) =
        let declared =
            match block.Tag with
            | Tick ->
                loadBlockDeclaredVariables
            | Load ->
                Map.empty
        List.fold (fun accu statement -> checkStatement accu TypeError statement) declared block.Statements |> ignore

    List.iter checkFunction program.Functions
    List.iter checkTaggedBlock program.TaggedBlocks

    let errorSequence = Seq.toList errors
    if List.isEmpty errorSequence then
        Ok program
    else
        Error errorSequence

