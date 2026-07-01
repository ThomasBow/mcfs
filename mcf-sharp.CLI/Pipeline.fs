



module Pipeline


let run (sourceCode: string) (nameSpace: string) (objective: string) (outputDir: string) : Result<unit, string list> =
    let tokens = Lexer.tokenize sourceCode
    match Parser.parse tokens with
    | Error errors ->
        Error (errors |> List.map string)
    | Ok ast -> 
        match Semantic.analyse ast with
        | Error errors ->
            Error (errors |> List.map string)
        | Ok program ->
            let irProgram = Lowering.lowerProgram program
            let files = Emitter.emitProgram irProgram nameSpace objective
            Writer.writeDatapack outputDir files
            Ok ()