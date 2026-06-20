


module Parser

open Token
open Ast

let parse (tokens: Token list) : Program = 
    let mutable i = 0
    let peek () = tokens[i]
    let advance () = let token = tokens[i] in i <- i + 1; token

    let expect token =
        if peek() = token then advance() |> ignore
        else failwithf "Expected %A but got %A" token (peek())

    let guardAgainstEOF () = if peek() = TEndOfFile then failwithf "Missing closing delimiter."
    let expectIdentifier () = 
        match advance() with
                | TIdentifier identifier -> identifier
                | token -> failwithf "Expected an identifier [A-z(0-9|A-z)*], but got '%A'" token


    let rec parseExpression () = parseComparison()

    and parseComparison () =
        let mutable left = parseAddSub()
        while peek() = TLessThan || peek() = TLessThanEqual || peek() = TGreaterThan || peek() = TGreaterThanEqual || peek() = TEqual || peek() = TNotEqual do
            let operator = 
                match advance() with
                    | TLessThan -> LessThan
                    | TLessThanEqual -> LessThanOrEqual
                    | TGreaterThan -> GreaterThan
                    | TGreaterThanEqual -> GreaterThanOrEqual
                    | TEqual -> Equals
                    | TNotEqual -> NotEquals
                    | token -> failwithf "Expected comparasion operator (< <=, >, >=, ==, !=), but got %A" token
            left <- BinaryOperator(operator, left, parseAddSub())
        left

    and parseAddSub () = 
        let mutable left = parseMultiplyDivide()
        while peek() = TPlus || peek() = TMinus do
            let operator = if advance() = TPlus then Add else Subtract
            left <- BinaryOperator(operator, left, parseMultiplyDivide())
        left

    and parseMultiplyDivide () = 
        let mutable left = parsePrimary()
        while peek() = TTimes || peek() = TDivide do
            let operator = if advance() = TTimes then Multiply else Divide
            left <- BinaryOperator(operator, left, parsePrimary())
        left

    and parsePrimary () = 
        match advance() with
            | TypeInt n -> IntLiteral n
            | TypeString s -> StringLiteral s
            | TIdentifier s -> Variable s
            | token -> failwithf "Unexpected token in expression: %A" token


    let rec parseStatement () = 
        match advance() with
            | TIdentifier identifier -> parseIdentifierStatement(identifier)
            | TIf -> parseIf()
            | TWhile -> parseWhile()
            | TReturn -> parseReturn()
            | TMcf -> parseMcf()
            | token -> failwithf "Unexpected token in statement: %A" token

    and parseIdentifierStatement (identifier: string) =
        match advance() with 
            | TColon -> parseVariable(identifier)
            | TLeftParenthesis -> parseFunctionCall(identifier)
            | token -> failwithf "Unexpect token '%A' for function call or variable declaration." token


    and parseVariable (identifier: string) = 
        let expression = if peek() <> TSemicolon then Some(parseExpression()) else None
        expect TSemicolon

        VariableDeclaration(identifier, expression)

    and parseIf () = 
        let condition = parseExpression()        
        let body = parseBlock()
        let elseBody = 
            if peek() = TElse then 
                advance() |> ignore
                Some(parseBlock()) 
            else None

        If(condition, body, elseBody)

    and parseWhile () =
        let condition = parseExpression() 
        let body = parseBlock()

        While(condition, body)

    and parseBlock () = 
        expect TLeftBrace
        let statements = System.Collections.Generic.List<Statement>()
        while peek() <> TRightBrace do
            guardAgainstEOF()
            statements.Add(parseStatement())
        advance() |> ignore

        Seq.toList statements

    and parseFunctionCall (identifier: string) =         
        let arguments = System.Collections.Generic.List<Expression>()
        while peek() <> TRightParenthesis do
            guardAgainstEOF()
            arguments.Add(parseExpression())
            if peek() = TComma then advance() |> ignore
        advance() |> ignore

        FunctionCall(identifier, Seq.toList arguments)
         
    and parseReturn () =
        let expression = if peek() <> TSemicolon then Some(parseExpression()) else None
        expect TSemicolon
    
        Return(expression)

    and parseMcf () =
        let str = 
            match advance() with
                | TypeString s -> s
                | token -> failwithf "Expected a string for raw command, but got %A" token
        expect TSemicolon
        RawCommand str
               

    let parseTaggedBlock () = 
        let tag = 
            match advance() with
                | TTick -> Tick
                | TLoad -> Load
                | token -> failwithf "Expected a tag (tick or load), but got %A" token
        let blockBody = parseBlock()

        { Tag = tag; Statements = blockBody }

    let parseFunctionDefinition () =
        let functionName = expectIdentifier()
            
        expect TLeftParenthesis
        let parameters = System.Collections.Generic.List<string>()
        while peek() <> TRightParenthesis do
            guardAgainstEOF()
            parameters.Add(expectIdentifier())
            if peek() = TComma then advance() |> ignore
        expect TRightParenthesis
        let body = parseBlock()

        { Name = functionName; Parameters = Seq.toList parameters; Body = body}


    let functions = System.Collections.Generic.List<FunctionDefinition>()
    let taggedBlocks = System.Collections.Generic.List<TaggedBlock>()
    while peek() <> TEndOfFile do 
        match advance() with 
            | TFunction -> functions.Add(parseFunctionDefinition())
            | TSnabelA -> taggedBlocks.Add(parseTaggedBlock())
            | token -> failwithf "Only function or tagged blocks can be declared at the top level '%A'" token

    { Functions = Seq.toList functions; TaggedBlocks = Seq.toList taggedBlocks }