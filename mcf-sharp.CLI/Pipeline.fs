



module Pipeline


let run (sourceCode: string) (nameSpace: string) (objective: string) (outputDir: string) : Result<unit, string list> =
    let tokens = Lexer.tokenize sourceCode
    let ast = Parser.parse tokens

    match Semantic.analyse ast with
    | Error errors ->
        Error (errors |> List.map string)
    | Ok program ->
        let irProgram = Lowering.lowerProgram program
        let files = Emitter.emitProgram irProgram nameSpace objective
        Writer.writeDatapack outputDir files
        Ok ()