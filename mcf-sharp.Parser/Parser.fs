


module Parser

open Token
open Ast

let formatError(message: string) (position: Position) =
    $"{message} (Location {position.Line}:{position.Column})"



let parse (tokens: PositionedToken list) : Program = 
    let mutable i = 0
    let peek () = tokens[i].Token
    let advance () = let token = tokens[i].Token in i <- i + 1; token

    let rest () = tokens[i..tokens.Length - 1]

    let currentPosition () = 
        tokens[i].Position
    let node value = { Value = value; Position = currentPosition() }

    let expect token =
        if peek() = token then advance() |> ignore
        else failwithf "Expected %A but got %A (Location %i:%i)" token (peek()) (currentPosition().Line) (currentPosition().Column)

    let guardAgainstEOF () = if peek() = TEndOfFile then failwithf "Missing closing delimiter."
    let parseIdentifier () = 
        let pos = currentPosition()
        match advance() with
                | TIdentifier identifier -> identifier
                | token -> failwith (formatError $"Expected an identifier [a-z(0-9|a-z|-|_)*], but got {token}" pos) 

    let parseType () = 
        let pos = currentPosition()
        match advance() with 
                | TInt -> TypeInt
                | TString -> TypeString
                | TBool -> TypeBool
                | TVoid -> TypeVoid
                | token -> failwith (formatError $"Expected type, but got {token}" pos)

    let parseTag () =
        let pos = currentPosition()
        match advance() with
            | TTick -> Tick 
            | TLoad -> Load 
            | token -> failwith (formatError $"Expected a tag (tick or load), but got {token}" pos)

    let rec parseExpression () = parseComparison()

    and parseComparison () =
        let pos = currentPosition();
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
                    | token -> failwith (formatError $"Expected comparasion operator (< <=, >, >=, ==, !=), but got {token}" pos)
            left <- node(BinaryOperator(operator, left, parseAddSub()))
        left

    and parseAddSub () = 
        let mutable left = parseMultiplyDivide()
        while peek() = TPlus || peek() = TMinus do
            let operator = if advance() = TPlus then Add else Subtract
            left <- node(BinaryOperator(operator, left, parseMultiplyDivide()))
        left

    and parseMultiplyDivide () = 
        let mutable left = parsePrimary()
        while peek() = TTimes || peek() = TDivide do
            let operator = 
                match advance() with 
                | TTimes -> Multiply
                | _ -> Divide
            left <- node(BinaryOperator(operator, left, parsePrimary()))
        left

    and parsePrimary () : Node<Expression> = 
        let pos = currentPosition()
        match advance() with
            | TIntLiteral n -> node(IntLiteral n)
            | TStringLiteral s -> node(StringLiteral s)
            | TIdentifier s -> node(Variable s)
            | token -> failwith (formatError $"Unexpected token in expression: {token}" pos)


    let rec parseStatement () =
        let pos = currentPosition()
        match advance() with
            | TIdentifier identifier -> parseIdentifierStatement(identifier)
            | TIf -> parseIf()
            | TWhile -> parseWhile()
            | TReturn -> parseReturn()
            | TMcf -> parseMcf()
            | token -> failwith (formatError $"Unexpected token in statement: {token}" pos)

    and parseIdentifierStatement (identifier: string) =
        let pos = currentPosition()
        match peek() with 
            | TColon -> parseVariableDeclaration(identifier)
            | TEqual -> parseVariableAssignment(identifier)
            | TLeftParenthesis -> parseFunctionCall(identifier)
            | token -> failwith (formatError $"Unexpect token {token}, expected function call '(' or variable declaration ':'." pos)

    // Format:
    // Identifier ":" Type ( "=" Expression )? ";" 
    and parseVariableDeclaration (identifier: string) = 
        let pos = currentPosition()
        let typeHint = 
            match advance() with
            | TColon -> 
                match advance() with
                | TInt -> 
                    Some(TypeInt)
                | TString ->              
                    Some(TypeString)
                | TBool ->
                    Some(TypeBool)                
                | _ -> None
            | _ -> None
                
        let expression =           
            match advance() with
            | TEqual -> 
                let expression = Some(parseExpression())
                expect TSemicolon
                expression
            | TSemicolon -> None
            | token -> failwith (formatError $"Expected expression or ';', but got {token}" pos)

        VariableDeclaration(identifier, typeHint, expression)

    // Format:
    // Identifier "=" Expression ";"
    and parseVariableAssignment (identifier: string) =
        expect TEqual
        let expression = parseExpression()
        expect TSemicolon

        VariableAssignment(identifier, expression)

    // Format:
    // "if" Expression Block ("else" Block )?
    and parseIf () = 
        let condition = parseExpression()        
        let body = parseBlock()
        let elseBody = 
            if peek() = TElse then 
                expect TElse
                Some(parseBlock()) 
            else None

        If(condition, body, elseBody)

    // Format:
    // "while" Expression Block
    and parseWhile () =
        let condition = parseExpression() 
        let body = parseBlock()

        While(condition, body)

    // Format:
    // "{" Statement* "}"
    and parseBlock () = 
        expect TLeftBrace
        let statements = System.Collections.Generic.List<Node<Statement>>()
        while peek() <> TRightBrace do
            guardAgainstEOF()
            statements.Add(node(parseStatement()))
        expect TRightBrace

        Seq.toList statements

    and parseFunctionCall (identifier: string) =   
        expect(TLeftParenthesis)
        let arguments = System.Collections.Generic.List<Node<Expression>>()
        while peek() <> TRightParenthesis do
            guardAgainstEOF()
            arguments.Add(parseExpression())
            if peek() = TComma then advance() |> ignore
        advance() |> ignore

        FunctionCall(identifier, Seq.toList arguments)
         
    // Format:
    // "return" Expression?
    and parseReturn () =
        let expression = if peek() <> TSemicolon then Some(parseExpression()) else None
        expect TSemicolon
    
        Return(expression)

    and parseMcf () =
        let pos = currentPosition()
        let str = 
            match advance() with
                | TStringLiteral s -> s
                | token -> failwith (formatError $"Expected a string for raw command, but got {token}" pos)
        expect TSemicolon
        RawCommand str
               
    // Format:
    // "@" Tag Block 
    let parseTaggedBlock () = 
        expect(TSnabelA)
        let tag = parseTag()
        let blockBody = parseBlock()

        { Tag = tag; Statements = blockBody }

    // Format:
    // "fn" Type Identifier "(" ( ( Identifier ":" Type "," )* Identifier ":" Type )? ")" Block
    let parseFunctionDefinition () =
        expect(TFunction)
        let returnType = parseType()            
        let functionName = String.map System.Char.ToLower (parseIdentifier())       
        expect TLeftParenthesis

        let parameters = System.Collections.Generic.List<Parameter>()
        while peek() <> TRightParenthesis do
            guardAgainstEOF()
            let parameterName = parseIdentifier()
            expect(TColon)
            let typeHint = parseType()
            if peek() <> TRightParenthesis then expect(TComma)

            parameters.Add({ Name = parameterName; Type = typeHint })

        expect TRightParenthesis
        let body = parseBlock()

        { Name = functionName; Parameters = Seq.toList parameters; Body = body; ReturnType = returnType }


    let functions = System.Collections.Generic.List<FunctionDefinition>()
    let taggedBlocks = System.Collections.Generic.List<TaggedBlock>()
    while peek() <> TEndOfFile do 
        let pos = currentPosition()
        match peek() with 
            | TFunction -> functions.Add(parseFunctionDefinition())
            | TSnabelA -> taggedBlocks.Add(parseTaggedBlock())
            | token -> failwith (formatError $"Only function or tagged blocks can be declared at the top level {token}" pos)

    { Functions = Seq.toList functions; TaggedBlocks = Seq.toList taggedBlocks }