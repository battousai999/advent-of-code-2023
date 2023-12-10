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

type Path = {
    OriginX: int
    OriginY: int
    PipeX: int
    PipeY: int
    Pipe: PipeType
}

let rawMap = File.ReadAllLines("map.txt")

// let rawMapStr = @"..F7.
// .FJ|.
// SJ.L7
// |F--J
// LJ..."

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

let find2D value (arr: 'T [,]) =
    let rec check x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then check 0 (y+1)
          elif arr.[x,y] = value then Some (x,y)
          else check (x+1) y
    check 0 0

let safeGetValue (x: int, y: int) =
    if (x >= 0) && (x < xSize) && (y >= 0) && (y < ySize) then
        Some map[x,y]
    else
        None

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
        | Some pt when pt <> PipeType.Empty && pt <> PipeType.StartingPosition -> pt
        | _ -> raise <| ApplicationException(sprintf "invalid map location or pipe type: %A, %A" pipeType destinationEndpoint)
    let (destinationX, destinationY) = destinationEndpoint

    { OriginX = path.PipeX; OriginY = path.PipeY; Pipe = destinationPipeType; PipeX = destinationX; PipeY = destinationY }

let arePathsAdjacentOrEqual (path1: Path) (path2: Path) =
    let isAdjacent = (abs (path1.PipeX - path2.PipeX) = 1 && path1.PipeY = path2.PipeY) || (path1.PipeX = path2.PipeX && abs (path1.PipeY - path2.PipeY) = 1)
    (path1.PipeX, path1.PipeY) = (path2.PipeX, path2.PipeY) || isAdjacent

let rec followPath (path1: Path) (path2: Path) (steps: int) =
    let newPath1 = calculateNextStep path1
    let newPath2 = calculateNextStep path2

    if arePathsAdjacentOrEqual newPath1 newPath2 then
        steps + 1
    else
        followPath newPath1 newPath2 (steps + 1)

let stepsToFarthestPoint =
    let ((pipe1X, pipe1Y, pipe1), (pipe2X, pipe2Y, pipe2)) = startingAdjacentPipes
    let (startingX, startingY) = startingPosition
    let path1 = { OriginX = startingX; OriginY = startingY; Pipe = pipe1; PipeX = pipe1X; PipeY = pipe1Y }
    let path2 = { OriginX = startingX; OriginY = startingY; Pipe = pipe2; PipeX = pipe2X; PipeY = pipe2Y }

    followPath path1 path2 1

printfn "%d" stepsToFarthestPoint
