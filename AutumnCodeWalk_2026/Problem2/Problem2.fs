module Problem2

open System
open System.Collections.Generic
open System.Numerics
open LocalHelper

let parseContent (lines: string seq) =
    lines
    |> Seq.choose (fun line ->
        if String.IsNullOrWhiteSpace(line) then
            None
        else
            let parts = line.Split([|" -- "|], StringSplitOptions.None)
            Some(parts[0], parts[1])
    )

let longestPathLength (edges: (string * string) seq) =
    let nodeToId = Dictionary<string, int>()
    let mutable nextId = 0

    let getOrAddId node =
        match nodeToId.TryGetValue(node) with
        | true, id -> id
        | false, _ ->
            let id = nextId
            nextId <- nextId + 1
            nodeToId[node] <- id
            id

    let edgeIds =
        edges
        |> Seq.map (fun (a, b) -> (getOrAddId a, getOrAddId b))
        |> Seq.toArray

    let nodeCount = nextId
    if nodeCount = 0 then
        0
    else
        let neighbors = Array.init nodeCount (fun _ -> ResizeArray<int>())
        for (a, b) in edgeIds do
            neighbors[a].Add(b)
            neighbors[b].Add(a)

        let bfsFarthest (start: int) =
            let dist = Array.create nodeCount -1
            let queue = Queue<int>()
            dist[start] <- 0
            queue.Enqueue(start)
            let mutable farthest = start
            while queue.Count > 0 do
                let node = queue.Dequeue()
                if dist[node] > dist[farthest] then
                    farthest <- node
                for next in neighbors[node] do
                    if dist[next] < 0 then
                        dist[next] <- dist[node] + 1
                        queue.Enqueue(next)
            (farthest, dist[farthest])

        if edgeIds.Length = nodeCount - 1 then
            let (endpoint, _) = bfsFarthest 0
            snd (bfsFarthest endpoint)
        else

        let bit i = bigint.One <<< i
        let isVisited (mask: bigint) i = (mask &&& bit i) <> bigint.Zero
        let addVisited (mask: bigint) i = mask ||| bit i

        let memo = Dictionary<struct (int * bigint), int>()

        let rec dfs node mask =
            let key = struct (node, mask)
            match memo.TryGetValue(key) with
            | true, cached -> cached
            | false, _ ->
                let mutable bestTail = 0
                let nextNodes = neighbors[node]

                for i = 0 to nextNodes.Count - 1 do
                    let nextNode = nextNodes[i]
                    if not (isVisited mask nextNode) then
                        let candidate = 1 + dfs nextNode (addVisited mask nextNode)
                        if candidate > bestTail then
                            bestTail <- candidate

                memo[key] <- bestTail
                bestTail

        let mutable best = 0
        for start = 0 to nodeCount - 1 do
            let pathLen = dfs start (addVisited bigint.Zero start)
            if pathLen > best then
                best <- pathLen

        best

let execute =
    let isSample = false
    let path = if isSample then "Problem2/sample_input_2.txt" else "Problem2/input_2.txt"
    let edges = parseContent (GetLinesFromFile path)
    longestPathLength edges
