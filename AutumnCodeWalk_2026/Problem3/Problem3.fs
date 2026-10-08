module Problem3

open System
open System.Text
open LocalHelper

let decode (content: string) =
    let bytes = Convert.FromBase64String(content.Trim())
    Encoding.Latin1.GetString(bytes)

let readHeader (s: string) =
    let readLine (p: int) =
        let e = s.IndexOf('\n', p)
        s.Substring(p, e - p), e + 1
    let _, p0 = readLine 0
    let nStr, p1 = readLine p0
    let n = int (nStr.Trim())
    let offsets, _ =
        (([], p1), [ 1 .. n ])
        ||> List.fold (fun (acc, p) _ ->
            let line, next = readLine p
            (int (line.Trim()) :: acc, next))
    List.rev offsets

let rec skipWs (s: string) (i: int) =
    if i < s.Length && (s[i] = ' ' || s[i] = '\n') then skipWs s (i + 1) else i

let rec parse (s: string) (start: int) : int64 * int =
    let i = skipWs s start
    match s[i] with
    | '"' ->
        let rec loop j len =
            match s[j] with
            | '"' -> len, j + 1
            | '\\' -> loop (j + 2) (len + 1L)
            | _ -> loop (j + 1) (len + 1L)
        loop (i + 1) 0L
    | '{' ->
        let rec loop j total =
            let j = skipWs s j
            if s[j] = '}' then total + 3L, j + 1
            else
                let kf, j = parse s j
                let j = skipWs s j
                if not (s.Substring(j, 2) = "=>") then failwithf "Expected => at %d" j
                let vf, j = parse s (j + 2)
                loop j (total + kf * vf)
        loop (i + 1) 0L
    | '[' ->
        let j = skipWs s (i + 1)
        if s[j] = ']' then 1L, j + 1
        else
            let rec loop j total =
                let f, j = parse s j
                let j = skipWs s j
                match s[j] with
                | ';' -> loop (j + 1) (total + f)
                | ']' -> total + f + 1L, j + 1
                | c -> failwithf "Unexpected '%c' at %d" c j
            loop j 0L
    | _ when String.CompareOrdinal(s, i, "true", 0, 4) = 0 -> 1L, i + 4
    | _ when String.CompareOrdinal(s, i, "false", 0, 5) = 0 -> 0L, i + 5
    | _ when String.CompareOrdinal(s, i, "nil", 0, 3) = 0 -> 31L, i + 3
    | _ ->
        let mutable j = i
        if s[j] = '+' || s[j] = '-' then j <- j + 1
        while j < s.Length && Char.IsDigit s[j] do j <- j + 1
        abs (Int64.Parse(s.Substring(i, j - i))), j

let checksum (s: string) =
    readHeader s
    |> List.mapi (fun id off -> int64 id * fst (parse s off))
    |> List.sum

let execute =
    let isSample = false
    let path = if isSample then "Problem3/sample_input_3.txt" else "Problem3/input_3.txt"
    GetContentFromFile path |> decode |> checksum