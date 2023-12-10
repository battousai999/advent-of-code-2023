open System
open System.IO

type PipeType =
    | Vertical
    | Horizontal
    | NorthAndEast
    | NorthAndWest
    | SouthAndWest
    | SouthAndEast
    | StartingPosition
    | Empty
    | MarkedOutside

type Path = {
    OriginX: int
    OriginY: int
    PipeX: int
    PipeY: int
    Pipe: PipeType
}

let rawMap = File.ReadAllLines("../day-10-part-01/map.txt")

// let rawMapStr = @"..F7.
// .FJ|.
// SJ.L7
// |F--J
// LJ..."

// let rawMapStr = @"...........
// .S-------7.
// .|F-----7|.
// .||.....||.
// .||.....||.
// .|L-7.F-J|.
// .|..|.|..|.
// .L--J.L--J.
// ..........."

// let rawMapStr = @".F----7F7F7F7F-7....
// .|F--7||||||||FJ....
// .||.FJ||||||||L7....
// FJL7L7LJLJ||LJ.L-7..
// L--J.L7...LJS7F-7L7.
// ....F-J..F7FJ|L7L7L7
// ....L7.F7||L7|.L7L7|
// .....|FJLJ|FJ|F7|.LJ
// ....FJL-7.||.||||...
// ....L---J.LJ.LJLJ..."

// let rawMap = rawMapStr.Split(Environment.NewLine)

let xSize = (rawMap |> Array.head).Length
let ySize = rawMap |> Array.length

let mutable map = Array2D.init xSize ySize (fun _ _ -> PipeType.Empty)

rawMap
    |> Array.iteri (
        fun yIndex element ->
            element
                |> Seq.iteri (
                    fun xIndex ch ->
                        let pipeType =
                            match ch with
                            | '|' -> PipeType.Vertical
                            | '-' -> PipeType.Horizontal
                            | 'L' -> PipeType.NorthAndEast
                            | 'J' -> PipeType.NorthAndWest
                            | '7' -> PipeType.SouthAndWest
                            | 'F' -> PipeType.SouthAndEast
                            | '.' -> PipeType.Empty
                            | 'S' -> PipeType.StartingPosition
                            | _ -> raise <| ApplicationException($"invalid pipe type: {ch}")
                        map[xIndex, yIndex] <- pipeType))

let safeGetValue (x: int, y: int) =
    if (x >= 0) && (x < xSize) && (y >= 0) && (y < ySize) then
        Some map[x,y]
    else
        None

let find2D value (arr: 'T [,]) =
    let rec check x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then check 0 (y+1)
          elif arr.[x,y] = value then Some (x,y)
          else check (x+1) y
    check 0 0

let startingPosition =
    let result = map |> find2D PipeType.StartingPosition
    match result with
    | Some (x, y) -> (x, y)
    | None -> raise <| ApplicationException($"starting position not found")

let findAdjacentPipes (x: int, y: int) =
    let adjoiningPipes =
        let northPipe =
            safeGetValue(x, y-1) |> Option.map (fun p -> if p = PipeType.SouthAndWest || p = PipeType.SouthAndEast || p = PipeType.Vertical then Some (x, y-1, p) else None) |> Option.flatten
        let eastPipe =
            safeGetValue (x+1, y) |> Option.map (fun p -> if p = PipeType.NorthAndWest || p = PipeType.SouthAndWest || p = PipeType.Horizontal then Some (x+1, y, p) else None) |> Option.flatten
        let southPipe =
            safeGetValue (x, y+1) |> Option.map (fun p -> if p = PipeType.NorthAndWest || p = PipeType.NorthAndEast || p = PipeType.Vertical then Some (x, y+1, p) else None) |> Option.flatten
        let westPipe =
            safeGetValue (x-1, y) |> Option.map (fun p -> if p = PipeType.NorthAndEast || p = PipeType.SouthAndEast || p = PipeType.Horizontal then Some (x-1, y, p) else None) |> Option.flatten
        [northPipe; eastPipe; southPipe; westPipe] |> List.choose (fun x -> x)

    match adjoiningPipes with
    | [a; b] -> (a, b)
    | _ -> raise <| ApplicationException(sprintf "invalid pipe configuration adjoining the starting position: %A" adjoiningPipes)

let startingAdjacentPipes = findAdjacentPipes startingPosition

let calculateNextStep (path: Path) =
    let getEndpoints (pipe: PipeType) ((x, y): int * int) =
        match pipe with
        | PipeType.Horizontal -> [(x-1, y); (x+1, y)]
        | PipeType.Vertical -> [(x, y-1); (x, y+1)]
        | PipeType.NorthAndEast -> [(x, y-1); (x+1, y)]
        | PipeType.NorthAndWest -> [(x, y-1); (x-1, y)]
        | PipeType.SouthAndEast -> [(x, y+1); (x+1, y)]
        | PipeType.SouthAndWest -> [(x, y+1); (x-1, y)]
        | _ -> raise <| ApplicationException($"invalid calculation for endpoints: {pipe.ToString()}, (x={x}, y={y})")
    let destinationEndpoint =
        let endpoints = getEndpoints path.Pipe (path.PipeX, path.PipeY)
        let remainingEndpoints = endpoints |> List.filter (fun (x, y) -> x <> path.OriginX || y <> path.OriginY)
        match remainingEndpoints with
        | [destination] -> destination
        | _ -> raise <| ApplicationException(sprintf "invalid remaining endpoints at (pipe=%A, x=%d, y=%d): %A, %A, %A" path.Pipe path.PipeX path.PipeY remainingEndpoints endpoints path)
    let destinationPipeType =
        let pipeType = safeGetValue destinationEndpoint
        match pipeType with
        | Some pt when pt <> PipeType.Empty -> pt
        | _ -> raise <| ApplicationException(sprintf "invalid map location or pipe type: %A, %A" pipeType destinationEndpoint)
    let (destinationX, destinationY) = destinationEndpoint

    { OriginX = path.PipeX; OriginY = path.PipeY; Pipe = destinationPipeType; PipeX = destinationX; PipeY = destinationY }

let arePathsAdjacent  (path1: Path) (path2: Path) =
    (abs (path1.PipeX - path2.PipeX) = 1 && path1.PipeY = path2.PipeY) || (path1.PipeX = path2.PipeX && abs (path1.PipeY - path2.PipeY) = 1)

let arePathsEqual (path1: Path) (path2: Path) =
    (path1.PipeX, path1.PipeY) = (path2.PipeX, path2.PipeY)

let rec followPath (path1: Path) (path2: Path) (pathPoints: (int * int) * (int * int)) (mapPoints: (int * int) list) =
    let newPath1 = calculateNextStep path1
    let newPath2 = calculateNextStep path2
    let (pathPoint1, pathPoint2) = pathPoints
    let newPathPoint1 = (newPath1.PipeX, newPath1.PipeY)
    let newPathPoint2 = (newPath2.PipeX, newPath2.PipeY)

    if arePathsAdjacent newPath1 newPath2 then
        (newPath1.PipeX, newPath1.PipeY) :: (newPath2.PipeX, newPath2.PipeY) :: pathPoint1 :: pathPoint2 :: mapPoints
    elif arePathsEqual newPath1 newPath2 then
        (newPath1.PipeX, newPath1.PipeY) :: pathPoint1 :: pathPoint2 :: mapPoints
    else
        followPath newPath1 newPath2 (newPathPoint1, newPathPoint2) (pathPoint1 :: pathPoint2 :: mapPoints)

let pathPoints =
    let ((pipe1X, pipe1Y, pipe1), (pipe2X, pipe2Y, pipe2)) = startingAdjacentPipes
    let (startingX, startingY) = startingPosition
    let path1 = { OriginX = startingX; OriginY = startingY; Pipe = pipe1; PipeX = pipe1X; PipeY = pipe1Y }
    let path2 = { OriginX = startingX; OriginY = startingY; Pipe = pipe2; PipeX = pipe2X; PipeY = pipe2Y }
    let pathPoint1 = (path1.PipeX, path1.PipeY)
    let pathPoint2 = (path2.PipeX, path2.PipeY)

    followPath path1 path2 (pathPoint1, pathPoint2) [startingPosition]

let draw () =
    let output =
        [0..(ySize-1)] |> List.map (
            fun y ->
                [0..(xSize-1)]
                    |> List.map (
                        fun x ->
                            let isOnPath = pathPoints |> List.contains (x, y)

                            match safeGetValue (x, y) with
                            | Some PipeType.Vertical -> if isOnPath then '┃' else '│'
                            | Some PipeType.Horizontal -> if isOnPath then '━' else '─'
                            | Some PipeType.NorthAndEast -> if isOnPath then '┗' else '└'
                            | Some PipeType.NorthAndWest -> if isOnPath then '┛' else '┘'
                            | Some PipeType.SouthAndEast -> if isOnPath then '┏' else '┌'
                            | Some PipeType.SouthAndWest -> if isOnPath then '┓' else '┐'
                            | Some PipeType.Empty -> '•'
                            | Some PipeType.StartingPosition -> 'S'
                            | Some PipeType.MarkedOutside -> '○'
                            | None -> '?')
                    |> String.Concat)
    output |> List.iter (fun s -> printfn "%s" s)

printfn ""
printfn "Initial Map:"
draw ()

let convertNonPathToEmpty () =
    let allPoints = [for x in [0..xSize-1] do for y in [0..ySize-1] do (x, y)]
    let nonPathPoints = allPoints |> List.filter (fun (x, y) -> not (pathPoints |> List.contains (x, y)))

    nonPathPoints |> List.iter (fun (x, y) -> map[x, y] <- PipeType.Empty)

convertNonPathToEmpty ()

printfn ""
printfn "After Converting Non-Path Points to Empty:"
draw()

let traverseClockwiseMarkingOnLeft () =
    let startingPoint =
        let result =
            [0..ySize-1]
                |> List.tryPick (
                    fun y ->
                        [0..xSize-1]
                            |> List.tryPick (
                                fun x ->
                                    match safeGetValue (x, y) with
                                    | Some PipeType.Empty
                                    | Some PipeType.MarkedOutside -> None
                                    | Some PipeType.SouthAndEast -> Some (x, y)
                                    | _ -> raise <| ApplicationException(sprintf "unexpected pipe type at (%d, %d): %A" x y (safeGetValue (x, y)))))
        match result with
        | Some pt -> pt
        | None -> raise <| ApplicationException("cannot find starting point")

    let (startingPointX, startingPointY) = startingPoint
    let startingPath = { OriginX = startingPointX; OriginY = startingPointY + 1; Pipe = PipeType.SouthAndEast; PipeX = startingPointX; PipeY = startingPointY }

    let rec traverse (path: Path) =
        let startingAdjacentPoints =
            let ((point1X, point1Y, _), (point2X, point2Y, _)) = startingAdjacentPipes
            (point1X, point1Y), (point2X, point2Y)
        let nextPath =
            match path.Pipe with
            | PipeType.StartingPosition ->
                let (nextPointX, nextPointY) =
                    match startingAdjacentPoints with
                    | ((x1, y1), (x2, y2)) when x1 = path.OriginX && y1 = path.OriginY -> (x2, y2)
                    | ((x1, y1), (x2, y2)) when x2 = path.OriginX && y2 = path.OriginY -> (x1, y1)
                    | _ -> raise <| ApplicationException(sprintf "cannot determine next point for starting point: %A, startingAdjactentPoints = %A" path startingAdjacentPoints)
                { OriginX = path.PipeX; OriginY = path.PipeY; Pipe = safeGetValue (nextPointX, nextPointY) |> Option.get; PipeX = nextPointX; PipeY = nextPointY }
            | _ -> calculateNextStep path
        let leftPoints =
            let nextPathX = nextPath.PipeX
            let nextPathY = nextPath.PipeY
            let isNorth (x, y) = x = path.PipeX && y = path.PipeY - 1
            let isEast (x, y) = x = path.PipeX + 1 && y = path.PipeY
            let isSouth (x, y) = x = path.PipeX && y = path.PipeY + 1
            let isWest (x, y) = x = path.PipeX - 1 && y = path.PipeY
            let effectivePipeType =
                match path.Pipe with
                | PipeType.Empty
                | PipeType.MarkedOutside -> raise <| ApplicationException(sprintf "unexpected pipe type at (%d, %d): %A" path.PipeX path.PipeY path.Pipe)
                | PipeType.StartingPosition ->
                    match startingAdjacentPoints with
                    | ((x1, y1), (x2, y2)) when x1 = x2 && abs (y1 - y2) = 2 -> PipeType.Vertical
                    | ((x1, y1), (x2, y2)) when y1 = y2 && abs (x1 - x2) = 2 -> PipeType.Horizontal
                    | (point1, point2) when (isNorth point1 && isEast point2) || (isNorth point2 && isEast point1) -> PipeType.NorthAndEast
                    | (point1, point2) when (isNorth point1 && isWest point2) || (isNorth point2 && isWest point1) -> PipeType.NorthAndWest
                    | (point1, point2) when (isSouth point1 && isEast point2) || (isSouth point2 && isEast point1) -> PipeType.SouthAndEast
                    | (point1, point2) when (isSouth point1 && isWest point2) || (isSouth point2 && isWest point1) -> PipeType.SouthAndWest
                    | _ -> raise <| ApplicationException(sprintf "cannot determine effective path type at (%d, %d): %A" path.PipeX path.PipeY startingAdjacentPipes)
                | _ -> path.Pipe

            match effectivePipeType with
            | PipeType.Vertical ->
                if nextPathY < path.PipeY then  // travelling north
                    [(path.PipeX - 1, path.PipeY)]
                else                            // travelling south
                    [(path.PipeX + 1, path.PipeY)]
            | PipeType.Horizontal ->
                if nextPathX > path.PipeX then  // travelling east
                    [(path.PipeX, path.PipeY - 1)]
                else                            // travelling west
                    [(path.PipeX, path.PipeY + 1)]
            | PipeType.NorthAndEast ->
                if nextPathY < path.PipeY then  // travelling north
                    [(path.PipeX, path.PipeY + 1); (path.PipeX - 1, path.PipeY + 1); (path.PipeX - 1, path.PipeY)]
                else                            // travelling east
                    []
            | PipeType.NorthAndWest ->
                if nextPathY < path.PipeY then  // travelling north
                    []
                else                            // travelling west
                    [(path.PipeX, path.PipeY + 1); (path.PipeX + 1, path.PipeY + 1); (path.PipeX + 1, path.PipeY)]
            | PipeType.SouthAndWest ->
                if nextPathY > path.PipeY then  // travelling south
                    [(path.PipeX, path.PipeY - 1); (path.PipeX + 1, path.PipeY - 1); (path.PipeX + 1, path.PipeY)]
                else                            // travelling west
                    []
            | PipeType.SouthAndEast ->
                if nextPathY > path.PipeY then  // travelling south
                    []
                else                            // travelling east
                    [(path.PipeX, path.PipeY - 1); (path.PipeX - 1, path.PipeY - 1); (path.PipeX - 1, path.PipeY)]
            | _ -> raise <| ApplicationException(sprintf "unexpected effective pipe type at (%d, %d): %A" path.PipeX path.PipeY effectivePipeType)

        // Mark left-side PipeType.Empty points as PipeType.MarkedOutside
        leftPoints
            |> List.filter (fun pt -> safeGetValue pt = Some PipeType.Empty)
            |> List.iter (fun (x, y) -> map[x, y] <- PipeType.MarkedOutside)

        if arePathsEqual nextPath startingPath then
            ()
        else
            traverse nextPath

    traverse startingPath

traverseClockwiseMarkingOnLeft ()

printfn ""
printfn "After Traversing Clockwise Marking Empty on Left-hand Side:"
draw ()

let markEmptyAdjacentToMarkedOutside () =
    let startingMarkedOutside = [for x in [0..xSize-1] do for y in [0..ySize-1] do if safeGetValue (x, y) = Some PipeType.MarkedOutside then (x, y)]
    let findAdjacentEmpty ((x, y): int * int) =
        let northPipe =
            safeGetValue(x, y-1) |> Option.map (fun p -> if p = PipeType.Empty then Some (x, y-1) else None) |> Option.flatten
        let eastPipe =
            safeGetValue (x+1, y) |> Option.map (fun p -> if p = PipeType.Empty then Some (x+1, y) else None) |> Option.flatten
        let southPipe =
            safeGetValue (x, y+1) |> Option.map (fun p -> if p = PipeType.Empty then Some (x, y+1) else None) |> Option.flatten
        let westPipe =
            safeGetValue (x-1, y) |> Option.map (fun p -> if p = PipeType.Empty then Some (x-1, y) else None) |> Option.flatten
        [northPipe; eastPipe; southPipe; westPipe] |> List.choose (fun x -> x)
    let rec markAdjacentEmpty ((x, y): int * int) =
        let adjacent = findAdjacentEmpty (x, y)

        match safeGetValue (x, y) with
        | Some PipeType.Empty -> map[x, y] <- PipeType.MarkedOutside
        | _ -> ()

        adjacent |> List.iter markAdjacentEmpty

    startingMarkedOutside |> List.iter markAdjacentEmpty

markEmptyAdjacentToMarkedOutside ()

printfn ""
printfn "After Marking Empty Adjacent to Marked Outside:"
draw ()

let innerCount =
    let allPoints = [for x in [0..xSize-1] do for y in [0..ySize-1] do (x, y)]

    allPoints
        |> List.sumBy (
            fun (x, y) ->
                match safeGetValue (x, y) with
                | Some PipeType.Empty -> 1
                | _ -> 0)

printfn ""
printfn "inner count = %d" innerCount
