


module Lexer
open Token
open Ast

let tokenize (source: string) : PositionedToken list = 
    let mutable i = 0
    let mutable line = 1
    let mutable column = 1
    let tokens = System.Collections.Generic.List<PositionedToken>()
    let skip (amount: int) = 
        i <- i + amount
        column <- column + amount
    let currentLocation() =
        { Line = line; Column = column }
    let addToken2 (token: Token) (position: Position) =
        tokens.Add { Token = token; Position = position }
    let addToken (token: Token) = 
        addToken2 token (currentLocation())
        
    while i < source.Length do
        match source[i] with
        // Ignore spaces tabs and newlines
        | ' ' | '\t' | '\r' ->
            skip 1

        | '\n' ->
            line <- line + 1
            column <- 1
            i <- i + 1
        
        // Basic tokens
        | '+' -> addToken TPlus; skip 1
        | '-' -> addToken TMinus; skip 1
        | '*' -> addToken TTimes; skip 1
        | '/' -> addToken TDivide; skip 1
        | '%' -> addToken TModulus; skip 1
        | '=' -> addToken TEqual; skip 1
        | ';' -> addToken TSemicolon; skip 1
        | '{' -> addToken TLeftBrace; skip 1
        | '}' -> addToken TRightBrace; skip 1
        | '(' -> addToken TLeftParenthesis; skip 1
        | ')' -> addToken TRightParenthesis; skip 1
        | ',' -> addToken TComma; skip 1

        | '@' -> 
            let pos = currentLocation()
            addToken2 TSnabelA pos
            skip 5           
            let tag = source[i-4..i-1]
            match tag with
                | "load" -> addToken2 TLoad pos
                | "tick" -> addToken2 TTick pos
                | other -> failwithf "Expected tag (load or tick), but got %A" other

        | ':' -> 
            addToken TColon; skip 1

        | '<' ->
            skip 1
            match source[i] with
                | '=' -> addToken TLessThanEqual; skip 1
                | _ -> addToken TLessThan

        | '>' ->
            skip 1
            match source[i] with
                | '=' -> addToken TGreaterThanEqual; skip 1
                | _ -> addToken TGreaterThan

        // Integers - If current character is a digit go to the next none digit symbol, convert everything from current to next none digit - 1 into an integer
        | character when System.Char.IsDigit(character) ->
            let pos = currentLocation()
            let start = i
            while i < source.Length && System.Char.IsDigit(source[i]) do
                skip 1
            addToken2 (TIntLiteral(int source[start..i-1])) pos

        | '"' ->
            let pos = currentLocation()
            skip 1
            let start = i
            while i < source.Length && source[i] <> '"' do
                skip 1
            let str = source[start..i-1]
            addToken2 (TStringLiteral str) pos
            skip 1

        // Identifiers - If current character is a letter, go to next none letter or digit, convert everything from current to next none letter/digit - 1 into a keyword/identifier 
        | character when System.Char.IsLetter(character) ->
            let pos = currentLocation()
            let start = i
            while i < source.Length && (System.Char.IsLetterOrDigit(source[i]) || source[i] = '-' || source[i] = '_') do
                skip 1
            let word = source[start..i-1]
            addToken2(
                match word with                    
                    | "fn" -> TFunction
                    | "if" -> TIf
                    | "else" -> TElse
                    | "while" -> TWhile
                    | "return" -> TReturn
                    | "mcf" -> TMcf
                    | "int" -> TInt
                    | "string" -> TString
                    | "bool" -> TBool
                    | "void" -> TVoid
                    | _ -> TIdentifier word
            ) pos      

        | c -> failwithf "Unexpected character '%c' (ASCII %d) at position %d" c (int c) i

    addToken(TEndOfFile)
    tokens |> Seq.toList