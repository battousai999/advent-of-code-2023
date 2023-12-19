open System
open System.IO
open System.Collections.Generic

type Point = {
    X: int
    Y: int
}

type Path = {
    Points: Point list
    TotalHeatLoss: int
}

// let rawMap = File.ReadAllLines("heatloss-map.txt")

let rawMapStr = @"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"

let rawMap = rawMapStr.Split(Environment.NewLine)

let sizeX = rawMap |> Array.head |> Seq.length
let sizeY = rawMap |> Array.length

let heatMap = Array2D.create sizeX sizeY 0

rawMap
    |> Array.iteri (
        fun yIndex line ->
            line
                |> Seq.iteri (
                    fun xIndex ch ->
                        let heatloss = Int32.Parse (ch |> string)
                        heatMap[xIndex, yIndex] <- heatloss))

let draw (map: int array2d) =
    seq {0..sizeY-1}
        |> Seq.iter (
            fun yIndex ->
                let lineOfChars =
                    seq {0..sizeX-1}
                        |> Seq.map (
                            fun xIndex ->
                                let heatloss = map[xIndex, yIndex]
                                (('0' |> int) + heatloss) |> char)
                let line = lineOfChars |> String.Concat
                printfn "%s" line)

draw heatMap

let endingPointOfPath (path: Path) = path.Points |> List.head
let isDisallowedStraight (point1: Point) (point2: Point) (point3: Point option) (point4: Point option) =
    if (Option.isNone point3) || (Option.isNone point4) then
        false
    else
        (point1.X = point2.X && point2.X = (Option.get point3).X && (Option.get point3).X = (Option.get point4).X) ||
        (point1.Y = point2.Y && point2.Y = (Option.get point3).Y && (Option.get point3).Y = (Option.get point4).Y)

let nextPointsOfPath (path: Path) =
    let endpoint = endingPointOfPath path
    let fromPoint = if (path.Points |> List.length) > 1 then Some (path.Points |> List.skip 1 |> List.head) else None
    let fromFromPoint = if (path.Points |> List.length) > 2 then Some (path.Points |> List.skip 2 |> List.head) else None
    let initalNextPoints = [
        { X = endpoint.X; Y = endpoint.Y - 1 }  // north
        { X = endpoint.X - 1; Y = endpoint.Y }  // west
        { X = endpoint.X; Y = endpoint.Y + 1 }  // south
        { X = endpoint.X + 1; Y = endpoint.Y }  // east
    ]
    let pointBacktracks (point: Point) = fromPoint |> Option.contains point

    initalNextPoints
        |> List.filter (
            fun point ->
                not (pointBacktracks point) &&
                point.X >= 0 && point.X < sizeX &&
                point.Y >= 0 && point.Y < sizeY &&
                not (isDisallowedStraight point endpoint fromPoint fromFromPoint) &&
                not (path.Points |> List.contains point))

let findPath (map: int array2d) (startingPoint: Point) (endingPoint: Point) =
    let startingHeatloss = map[startingPoint.X, startingPoint.Y]
    let currentPaths = PriorityQueue<Path, int>()
    let startingPath = { Points = [startingPoint]; TotalHeatLoss = startingHeatloss }

    currentPaths.Enqueue(startingPath, startingHeatloss)

    let rec includeNextCheapestPath() =
        let cheapestPath = currentPaths.Dequeue()
        let cheapestHeatloss = cheapestPath.TotalHeatLoss

        //printfn "popped cheapest path = %A" cheapestPath

        if (endingPointOfPath cheapestPath) = endingPoint then
            cheapestPath
        else
            let nextPoints = nextPointsOfPath cheapestPath
            let nextPaths = nextPoints |> List.map (fun point -> { Points = point :: cheapestPath.Points; TotalHeatLoss = cheapestHeatloss + map[point.X, point.Y] })

            nextPaths |> List.iter (fun path -> currentPaths.Enqueue(path, path.TotalHeatLoss))
            includeNextCheapestPath()

    let resultPath = includeNextCheapestPath()

    (resultPath, currentPaths.Count)

let startingPoint = { X = 0; Y = 0 }
let endingPoint = { X = sizeX - 1; Y = sizeY - 1 }

let (bestPath, queueCount) = findPath heatMap startingPoint endingPoint

printfn ""
printfn "%A" bestPath.Points
printfn ""
printfn "queue size = %d" queueCount
printfn "heatloss = %d" bestPath.TotalHeatLoss
