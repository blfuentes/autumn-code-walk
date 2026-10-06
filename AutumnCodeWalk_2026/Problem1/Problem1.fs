module Problem1

open System
open LocalHelper

let compress (runs: string array) =
    let buildMaximals (run: string) =
        if String.IsNullOrEmpty(run) then
            [||]
        else
            let tokens = ResizeArray<string>()
            let mutable current = run[0]
            let mutable counter = 1

            for i = 1 to run.Length - 1 do
                if run[i] <> current then
                    tokens.Add(sprintf "%c%d" current counter)
                    current <- run[i]
                    counter <- 1
                else
                    counter <- counter + 1

            tokens.Add(sprintf "%c%d" current counter)
            tokens.ToArray()

    let shortestCompressedLength (run: string) =
        let maximalTokens = buildMaximals run
        let baseLength = maximalTokens |> Array.sumBy (fun token -> token.Length)

        if maximalTokens.Length = 0 then
            0
        else
            let occurrences = System.Collections.Generic.Dictionary<string, int>()

            for token in maximalTokens do
                let mutable currentCount = 0
                if occurrences.TryGetValue(token, &currentCount) then
                    occurrences[token] <- currentCount + 1
                else
                    occurrences[token] <- 1

            let replacementContributions =
                occurrences
                |> Seq.map (fun kvp ->
                    let tokenLength = kvp.Key.Length
                    let tokenOccurrences = kvp.Value
                    tokenLength - tokenOccurrences * (tokenLength - 1)
                )
                |> Seq.sort
                |> Seq.toArray

            let mutable bestLength = baseLength
            let mutable prefixContribution = 0
            let limit = min 26 replacementContributions.Length

            for i = 0 to limit - 1 do
                prefixContribution <- prefixContribution + replacementContributions[i]
                let candidateLength = baseLength + 2 + prefixContribution
                if candidateLength < bestLength then
                    bestLength <- candidateLength

            bestLength

    runs
    |> Array.mapi (fun index run -> (index + 1) * shortestCompressedLength run)
    |> Array.sum

let execute =
    let isSample = false
    let path = if isSample then "Problem1/sample_input_1.txt" else "Problem1/input_1.txt"
    let runs = GetLinesFromFile path
    compress runs
