



module Writer

open System.IO
open Emitter

let writeFile (outputDir: string) (file: EmittedFile) : unit =
    let fullPath = Path.Combine(outputDir, file.Path)
    let directory = Path.GetDirectoryName(fullPath)

    if not (Directory.Exists directory) then
        Directory.CreateDirectory directory |> ignore

    File.WriteAllText(fullPath, file.Content)

let isInsideDatapacksFolder (path: string) : bool =
    let fullPath = Path.GetFullPath(path)
    let parts = fullPath.Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
    parts |> Array.contains "datapacks"

let writeDatapack (outputDir: string) (files: EmittedFile list) : unit =
    if Directory.Exists outputDir then
        if not (isInsideDatapacksFolder outputDir) then
            failwith $"Refusing to delete '{outputDir}' — path does not appear to be inside a 'datapacks' folder"
        Directory.Delete(outputDir, recursive = true)

    Directory.CreateDirectory outputDir |> ignore

    files |> List.iter (writeFile outputDir)



