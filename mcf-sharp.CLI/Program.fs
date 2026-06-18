


[<EntryPoint>]
let main argv =    
    
    let source = System.IO.File.ReadAllText argv[0]
    let tokens = Lexer.tokenize source
    let program = Parser.parse tokens
    printfn "%A" program
    0