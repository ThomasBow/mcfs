



module Semantic

open Ast

type SemanticError =
    | UndeclaredVariable of string
    | UndeclaredFunction of string
    | ReturnOutsideFunction of string

let analyse (program: Program) : Result<Program, SemanticError list> =
    let functionNames =
        program.Functions
        |> List.map (fun f -> f.Name)
        |> Set.ofList

    let errors = System.Collections.Generic.List<SemanticError>()

    // Gather all variables declared in @load blocks
    let loadDeclaredVariables =
        program.TaggedBlocks
        |> List.filter (fun block -> block.Tag = Load)
        |> List.collect (fun block -> block.Statements)
        |> List.choose (fun statement ->
            match statement with
            | VariableDeclaration (name, _) -> Some name
            | _ -> None)
        |> Set.ofList

    let rec checkExpression (declared: Set<string>) (expression: Expression) =
        match expression with
            | IntLiteral _ -> ()
            | IntLiteral _ -> TypeInt
            | StringLiteral _ -> TypeString
            | Variable name ->
                if not (Set.contains name declared) then
                    errors.Add (UndeclaredVariable $"Undeclared Variable {name}")
            | BinaryOperator (_, left, right) ->
                checkExpression declared left
                checkExpression declared right
            | Call(name, arguments) ->
                if not (Set.contains name functionNames) then
                    errors.Add (UndeclaredFunction $"Undeclared Function {name}")
                List.iter (checkExpression declared) arguments
    
    let rec checkStatement (declared: Set<string>) (statement: Statement) : Set<string> =
        match statement with
            | VariableDeclaration(name, initializer) ->
                initializer |> Option.iter (checkExpression declared)
                Set.add name declared
            | VariableAssignment(name, value) ->
                if not (Set.contains name declared) then
                    errors.Add (UndeclaredVariable $"Undeclared Variable {name}")
                checkExpression declared value
                declared
            | If(condition, thenBranch, elseBranch) ->
                checkExpression declared condition
                List.fold checkStatement declared thenBranch |> ignore 
                elseBranch |> Option.iter (List.fold checkStatement declared >> ignore) 
                declared
            | While(condition, body) ->
                checkExpression declared condition
                List.fold checkStatement declared body |> ignore
                declared
            | FunctionCall(name, arguments) ->
                if not (Set.contains name declared) then 
                    errors.Add (UndeclaredFunction $"Undeclared Function {name}")
                List.iter (checkExpression declared) arguments
                declared
            | Return value ->
                value |> Option.iter (checkExpression declared)
                declared
            | RawCommand _ -> 
                declared

    let checkFunction (func: FunctionDefinition) =
        let declared = func.Parameters |> Set.ofList |> Set.union loadDeclaredVariables
        List.fold checkStatement declared func.Body |> ignore

    // Also check the tagged blocks
    let checkTaggedBlock (block: TaggedBlock) =
        let declared = Set.empty
        List.fold checkStatement declared block.Statements |> ignore

    List.iter checkFunction program.Functions
    List.iter checkTaggedBlock program.TaggedBlocks

    let errorSequence = Seq.toList errors
    if List.isEmpty errorSequence then
        Ok program
    else
        Error errorSequence

