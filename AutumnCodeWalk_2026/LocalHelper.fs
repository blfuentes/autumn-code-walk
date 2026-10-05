module LocalHelper

open System.IO

let GetLinesFromFile(path: string) =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + @"/" + path)

let GetContentFromFile(path: string) =
    File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + path)

let ReadLines(path: string) =
    File.ReadLines(__SOURCE_DIRECTORY__ + @"/" + path)

let FileExists(path: string) =
    File.Exists(__SOURCE_DIRECTORY__ + @"/" + path)

let WriteContentToFile(path: string, content: string) =
    File.WriteAllText(__SOURCE_DIRECTORY__ + @"/" + path, content)