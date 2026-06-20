


module Lexer
open Token

let tokenize (source: string) : Token list = 
    let mutable i = 0
    let tokens = System.Collections.Generic.List<Token>()

    while i < source.Length do
        match source[i] with
        // Ignore spaces tabs and newlines
        | ' ' | '\t' | '\n' | '\r' ->
            i <- i + 1
        
        // Basic tokens
        | '+' -> tokens.Add TPlus; i <- i + 1
        | '-' -> tokens.Add TMinus; i <- i + 1
        | '*' -> tokens.Add TTimes; i <- i + 1
        | '/' -> tokens.Add TDivide; i <- i + 1
        | '%' -> tokens.Add TModulus; i <- i + 1
        | '=' -> tokens.Add TEqual; i <- i + 1
        | ';' -> tokens.Add TSemicolon; i <- i + 1
        | '{' -> tokens.Add TLeftBrace; i <- i + 1
        | '}' -> tokens.Add TRightBrace; i <- i + 1
        | '(' -> tokens.Add TLeftParenthesis; i <- i + 1
        | ')' -> tokens.Add TRightParenthesis; i <- i + 1
        | ',' -> tokens.Add TComma; i <- i + 1

        | '@' -> 
            tokens.Add TSnabelA
            i <- i + 5           
            let tag = source[i-4..i-1]
            match tag with
                | "load" -> tokens.Add TLoad
                | "tick" -> tokens.Add TTick
                | other -> failwithf "Expected tag (load or tick), but got %A" other

        | ':' -> 
            i <- i + 1
            match source[i] with
                | ':' -> tokens.Add TDoubleColon; i <- i + 1
                | _ -> tokens.Add TColon

        | '<' ->
            i <- i + 1
            match source[i] with
                | '=' -> tokens.Add TLessThanEqual; i <- i + 1
                | _ -> tokens.Add TLessThan

        | '>' ->
            i <- i + 1
            match source[i] with
                | '=' -> tokens.Add TGreaterThanEqual; i <- i + 1
                | _ -> tokens.Add TGreaterThan

        // Integers - If current character is a digit go to the next none digit symbol, convert everything from current to next none digit - 1 into an integer
        | character when System.Char.IsDigit(character) ->
            let start = i
            while i < source.Length && System.Char.IsDigit(source[i]) do
                i <- i + 1
            tokens.Add(TInt(int source[start..i-1]))

        | '"' ->
            i <- i + 1
            let start = i
            while i < source.Length && source[i] <> '"' do
                i <- i + 1
            let str = source[start..i-1]
            tokens.Add(TString str)
            i <- i + 1

        // Identifiers - If current character is a letter, go to next none letter or digit, convert everything from current to next none letter/digit - 1 into a keyword/identifier 
        | character when System.Char.IsLetter(character) ->
            let start = i
            while i < source.Length && System.Char.IsLetterOrDigit(source[i]) do
                i <- i + 1
            let word = source[start..i-1]
            tokens.Add(
                match word with                    
                    | "fn" -> TFunction
                    | "if" -> TIf
                    | "else" -> TElse
                    | "while" -> TWhile
                    | "return" -> TReturn
                    | "mcf" -> TMcf
                    | _ -> TIdentifier word
            )        

        | c -> failwithf "Unexpected character '%c' (ASCII %d) at position %d" c (int c) i

    tokens.Add(TEndOfFile)
    tokens |> Seq.toList