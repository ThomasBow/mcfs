


module Parser

open Token
open Ast


type ParseError =
    | Generic of string

let formatError(message: string) (position: Position) =
    $"{message} (Location {position.Line}:{position.Column})"

let parse (tokens: PositionedToken list) : Result<Program, ParseError list> = 
    let mutable i = 0
    let peek () = tokens[i].Token
    let advance () = let token = tokens[i].Token in i <- i + 1; token

    let currentPosition () = 
        tokens[i].Position
    let node value = { Value = value; Position = currentPosition() }

    let errors = System.Collections.Generic.List<ParseError>()

    let topLevelSynchronise () =
        while peek() <> TFunction && peek() <> TSnabelA && peek() <> TEndOfFile do
            advance() |> ignore

    let synchronise () =
        let mutable current = advance()
        while current <> TSemicolon && current <> TRightBrace && current <> TEndOfFile do
            current <- advance()

    let expect token =
        if peek() = token then advance() |> ignore
        else errors.Add (Generic ( formatError $"Expected {token} but got {peek()}" (currentPosition()) ))

    let guardAgainstEOF () = 
        let pos = currentPosition()
        if peek() = TEndOfFile 
            then errors.Add ( Generic ( formatError $"Missing closing delimiter." pos ))

    let parseType () = 
        let pos = currentPosition()
        match advance() with 
                | TInt -> TypeInt
                | TString -> TypeString
                | TBool -> TypeBool
                | TVoid -> TypeVoid
                | token -> 
                    errors.Add ( Generic ( formatError $"Expected type, but got {token}" pos )) 
                    ErrorType

    let parseIdentifier () =
        let pos = currentPosition()
        match advance() with 
                | TIdentifier identifier -> identifier;
                | token -> 
                    errors.Add ( Generic ( formatError $"Function declaration expected identifier in format:\n\"fn\" Type Identifier \"(\" ( ( Identifier \":\" Type \",\" )* Identifier \":\" Type )? \")\" Block" pos ))
                    "<error>"

    let parseTag () =
        let pos = currentPosition()
        match advance() with
            | TTick -> Tick 
            | TLoad -> Load 
            | token -> 
                errors.Add ( Generic ( formatError $"Expected a tag (tick or load), but got {token}" pos) )
                ErrorTag

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
                    | _ -> NotEquals
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
            | token -> 
                errors.Add ( Generic ( formatError $"Unexpected token in expression: {token}" pos))                
                node(ErrorExpression)


    let rec parseStatement () =
        let pos = currentPosition()
        match advance() with
            | TIdentifier identifier -> parseIdentifierStatement(identifier)
            | TIf -> parseIf()
            | TWhile -> parseWhile()
            | TReturn -> parseReturn()
            | TMcf -> parseMcf()
            | token -> 
                errors.Add ( Generic ( formatError $"Unexpected token in statement: {token}" pos ))
                synchronise()
                ErrorStatement

    and parseIdentifierStatement (identifier: string) =
        let pos = currentPosition()
        match peek() with 
            | TColon -> parseVariableDeclaration(identifier)
            | TEqual -> parseVariableAssignment(identifier)
            | TLeftParenthesis -> parseFunctionCall(identifier)
            | token -> 
                errors.Add ( Generic (formatError $"Unexpect token {token}, expected function call '(' or variable declaration ':'." pos))
                synchronise()
                ErrorStatement

    // Format:
    // Identifier ":" Type ( "=" Expression )? ";" 
    and parseVariableDeclaration (identifier: string) = 
        let pos = currentPosition()
        expect TColon
        let typeHint =             
            match advance() with
            | TInt -> 
                Some TypeInt
            | TString ->              
                Some TypeString
            | TBool ->
                Some TypeBool               
            | _ -> None
                
        let expression =           
            match advance() with
            | TEqual -> 
                let expression = Some (parseExpression())
                expect TSemicolon
                expression
            | TSemicolon -> None
            | token -> 
                errors.Add ( Generic (formatError $"Expected expression or ';', but got {token}" pos))
                Some (node(ErrorExpression))

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
        expect TRightParenthesis

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
        let functionName = parseIdentifier()   
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
            | token -> 
                errors.Add ( Generic ( formatError $"Only function or tagged blocks can be declared at the top level {token}" pos ))
                topLevelSynchronise()

    let errorSequence = Seq.toList errors;
    if List.isEmpty errorSequence
    then
        Ok { Functions = Seq.toList functions; TaggedBlocks = Seq.toList taggedBlocks }
    else 
        Error errorSequence