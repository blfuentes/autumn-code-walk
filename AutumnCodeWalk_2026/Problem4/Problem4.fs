module Problem4

let execute =
    let isSample = false
    let path = if isSample then "Problem4/sample_input_4.txt" else "Problem4/input_4.txt"

    let maze =
        System.IO.File.ReadAllLines(path)
        |> Array.filter (fun line -> line.Trim() <> "")
        |> Array.map (fun line -> line.Trim())

    let rowCount = maze.Length
    let colCount = maze[0].Length

    let findPosition marker =
        seq {
            for row in 0 .. rowCount - 1 do
                for col in 0 .. colCount - 1 do
                    if maze[row][col] = marker then
                        yield (row, col)
        }
        |> Seq.head

    let playerStart = findPosition 'X'
    let boxStart = findPosition '*'
    let targetPosition = findPosition 'T'

    // Convert (row, col) to a single integer so we can use fast arrays.
    let cellCount = rowCount * colCount
    let toCellId (row, col) = row * colCount + col

    // True if a cell is not a wall.
    let isOpenCell =
        Array.init cellCount (fun id ->
            let row = id / colCount
            let col = id % colCount
            maze[row][col] <> '#')

    // Direction order: up, down, left, right.
    let rowDelta = [| -1; 1; 0; 0 |]
    let colDelta = [| 0; 0; -1; 1 |]

    // Return adjacent cell in a direction, or -1 if outside grid / wall.
    let neighborCell cellId direction =
        let nextRow = cellId / colCount + rowDelta[direction]
        let nextCol = cellId % colCount + colDelta[direction]
        if nextRow >= 0 && nextRow < rowCount && nextCol >= 0 && nextCol < colCount then
            let nextId = nextRow * colCount + nextCol
            if isOpenCell[nextId] then nextId else -1
        else
            -1

    // Reusable arrays for BFS (player walking only, box treated as blocked).
    let visitStamp = Array.zeroCreate<int> cellCount
    let distanceFromStart = Array.zeroCreate<int> cellCount
    let bfsQueue = Array.zeroCreate<int> cellCount
    let mutable currentStamp = 0

    // Distance from player position to each side of the box.
    // Result index meaning: 0=up side, 1=down side, 2=left side, 3=right side.
    let walkingDistanceToBoxSides playerCell boxCell =
        currentStamp <- currentStamp + 1
        let sideDistances = Array.create 4 -1
        let sideCells = Array.init 4 (fun direction -> neighborCell boxCell direction)
        let mutable unresolvedSides = sideCells |> Array.filter (fun side -> side >= 0) |> Array.length

        let tryResolveSideDistance cell =
            for direction in 0 .. 3 do
                if sideCells[direction] = cell && sideDistances[direction] < 0 then
                    sideDistances[direction] <- distanceFromStart[cell]
                    unresolvedSides <- unresolvedSides - 1

        let mutable head = 0
        let mutable tail = 0

        visitStamp[playerCell] <- currentStamp
        distanceFromStart[playerCell] <- 0
        bfsQueue[tail] <- playerCell
        tail <- tail + 1
        tryResolveSideDistance playerCell

        while unresolvedSides > 0 && head < tail do
            let currentCell = bfsQueue[head]
            head <- head + 1

            for direction in 0 .. 3 do
                let nextCell = neighborCell currentCell direction
                if nextCell >= 0 && nextCell <> boxCell && visitStamp[nextCell] <> currentStamp then
                    visitStamp[nextCell] <- currentStamp
                    distanceFromStart[nextCell] <- distanceFromStart[currentCell] + 1
                    bfsQueue[tail] <- nextCell
                    tail <- tail + 1
                    tryResolveSideDistance nextCell

        sideDistances

    // Dijkstra over push states:
    // (box cell, direction of the latest push).
    let boxStartCell = toCellId boxStart
    let targetCell = toCellId targetPosition
    let bestCostByState = Array.create (cellCount * 4) System.Int32.MaxValue
    let priorityQueue = System.Collections.Generic.PriorityQueue<struct (int * int * int), int>()

    let expandPushes boxCell playerCell totalSteps =
        let sideDistances = walkingDistanceToBoxSides playerCell boxCell

        for pushDirection in 0 .. 3 do
            // To push in pushDirection, player must stand on opposite side first.
            let oppositeSide = pushDirection ^^^ 1
            let walkingSteps = sideDistances[oppositeSide]
            let nextBoxCell = neighborCell boxCell pushDirection

            if walkingSteps >= 0 && nextBoxCell >= 0 then
                let nextTotalSteps = totalSteps + walkingSteps + 1
                let stateId = nextBoxCell * 4 + pushDirection

                if nextTotalSteps < bestCostByState[stateId] then
                    bestCostByState[stateId] <- nextTotalSteps
                    priorityQueue.Enqueue(struct (nextBoxCell, pushDirection, nextTotalSteps), nextTotalSteps)

    let mutable minimumSteps = -1

    if boxStartCell = targetCell then
        minimumSteps <- 0
    else
        expandPushes boxStartCell (toCellId playerStart) 0

    while minimumSteps < 0 && priorityQueue.Count > 0 do
        let struct (boxCell, pushDirection, totalSteps) = priorityQueue.Dequeue()

        if totalSteps = bestCostByState[boxCell * 4 + pushDirection] then
            if boxCell = targetCell then
                minimumSteps <- totalSteps
            else
                // After a push, player stands where the box was right before that push.
                let playerCell = neighborCell boxCell (pushDirection ^^^ 1)
                expandPushes boxCell playerCell totalSteps

    minimumSteps
