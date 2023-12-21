open System
open System.IO

type CellType =
| StartingPoint
| Garden
| Rock
| VisitedGarden

type Point = {
    X: int
    Y: int
}

let rawMap = File.ReadAllLines("map.txt")

// let rawMapStr = @"...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ..........."

// let rawMap = rawMapStr.Split(Environment.NewLine)

let sizeX = rawMap |> Array.head |> Seq.length
let sizeY = rawMap |> Array.length

let map = Array2D.create sizeX sizeY CellType.Garden

rawMap
    |> Array.iteri (
        fun yIndex line ->
            line
                |> Seq.iteri (
                    fun xIndex ch ->
                        let cellType =
                            match ch with
                            | 'S' -> CellType.StartingPoint
                            | '.' -> CellType.Garden
                            | '#' -> CellType.Rock
                            | _ -> raise <| ApplicationException($"invalid cell type: {ch}")
                        map[xIndex, yIndex] <- cellType))

let drawMap (map: CellType array2d) (render: Point -> char -> char)=
    [0..sizeY-1]
        |> List.iter (
            fun yIndex ->
                let line =
                    [0..sizeX-1]
                        |> List.map (
                            fun xIndex ->
                                let ch =
                                    match map[xIndex, yIndex] with
                                    | CellType.StartingPoint -> 'S'
                                    | CellType.Rock -> '#'
                                    | CellType.Garden -> '.'
                                    | CellType.VisitedGarden -> '○'
                                render { X = xIndex; Y = yIndex } ch)
                        |> String.Concat

                printfn "%s" line)

drawMap map (fun _ ch -> ch)

let startingPoint =
    let yIndex = rawMap |> Array.findIndex (fun line -> line.Contains('S'))
    let xIndex = rawMap[yIndex] |> Seq.findIndex (fun ch -> ch = 'S')
    { X = xIndex; Y = yIndex }

let getNeighboringUnvisitedGardens (map: CellType array2d) (point: Point) =
    let neighbors = [
        { X = point.X; Y = point.Y - 1 };
        { X = point.X + 1; Y = point.Y };
        { X = point.X; Y = point.Y + 1 };
        { X = point.X - 1; Y = point.Y }
    ]
    let validNeighbors = neighbors |> List.filter (fun point -> point.X >= 0 && point.X < sizeX && point.Y >= 0 && point.Y < sizeY)

    validNeighbors
        |> List.filter (
            fun point ->
                let cellType = map[point.X, point.Y]
                cellType = CellType.Garden || cellType = CellType.StartingPoint)

let rec getDestinationGardens (positions: Point list) (steps: int) =
    if steps = 0 then
        positions
    else
        let newPositions = positions |> List.collect (getNeighboringUnvisitedGardens map) |> List.distinct
        getDestinationGardens newPositions (steps - 1)

let targetGardens = getDestinationGardens [startingPoint] 64

printfn ""
drawMap map (fun point ch -> if targetGardens |> List.contains point then '○' else ch)

printfn ""
printfn "%d" (targetGardens |> List.length)
