



module Program

open Pipeline

[<EntryPoint>]
let main argv =
    match argv with
    | [| sourcePath; outputDir; nameSpace |] ->
        let sourceCode = System.IO.File.ReadAllText sourcePath

        match run sourceCode nameSpace "vars" outputDir with
        | Ok () ->
            printfn "Datapack generated successfully at %s" outputDir
            0
        | Error errors ->
            errors |> List.iter (printfn "Error: %s")
            1
    | _ ->
        printfn "Usage: mcfsharp <source.mcf> <outputDir> <namespace>"
        1