module Problem0

open System.IO
open LocalHelper

let parseContent (lines: string array) =
      let parts = 
        lines[0].Split(' ') 
        |> Array.mapi(fun i s -> (i, s))

      (parts, lines[1 ..])

let sumIndexes (parts: (int*string) array) (toFind: string array) =
    let words = parts |> Array.map snd
    let haystack = System.String.Concat(words)

    let charToWordIndex =
        let map = Array.zeroCreate haystack.Length
        let mutable position = 0
        for i = 0 to words.Length - 1 do
            let wordLength = words[i].Length
            for j = 0 to wordLength - 1 do
                map[position + j] <- i
            position <- position + wordLength
        map

    toFind
    |> Array.sumBy(fun s ->
        let startPosition = haystack.IndexOf(s, System.StringComparison.Ordinal)
        if startPosition < 0 then -1
        else charToWordIndex[startPosition]
    )

let execute =
    let isSample = false
    let path = if isSample then "Problem0/sample_input_0.txt" else "Problem0/input_0.txt"
    let (parts, toFind) = parseContent (GetLinesFromFile path)
    sumIndexes parts toFind