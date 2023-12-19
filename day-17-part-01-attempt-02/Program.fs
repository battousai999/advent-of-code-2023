open System
open System.IO
open System.Linq
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

let draw (map: int array2d) (renderer: (int -> int -> char) option) =
    seq {0..sizeY-1}
        |> Seq.iter (
            fun yIndex ->
                let lineOfChars =
                    seq {0..sizeX-1}
                        |> Seq.map (
                            fun xIndex ->
                                match renderer with
                                | Some render ->
                                    render xIndex yIndex
                                | None ->
                                    let heatloss = map[xIndex, yIndex]
                                    (('0' |> int) + heatloss) |> char)
                let line = lineOfChars |> String.Concat
                printfn "%s" line)

draw heatMap None

// function Dijkstra(Graph, source):
//     dist[source] ← 0                           // Initialization
//     create vertex priority queue Q
//     for each vertex v in Graph.Vertices:
//         if v ≠ source
//             dist[v] ← INFINITY                 // Unknown distance from source to v
//             prev[v] ← UNDEFINED                // Predecessor of v
//
//     Q.add_with_priority(source, dist[source])
//
//     while Q is not empty:                      // The main loop
//         u ← Q.extract_min()                    // Remove and return best vertex
//         if u = target:
//             break out of while loop
//         for each neighbor v of u:              // Go through all v neighbors of u
//             alt ← dist[u] + Graph.Edges(u, v)
//             if alt < dist[v]:
//                 dist[v] ← alt
//                 prev[v] ← u
//                 if not Q.contains(v):
//                     Q.add_with_priority(v, alt)
//     return dist, prev

// -- To get shortest path
// S ← empty sequence
// u ← target
// if prev[u] is defined or u = source:          // Do something only if the vertex is reachable
//     while u is defined:                       // Construct the shortest path with a stack S
//         insert u at the beginning of S        // Push the vertex onto the stack
//         u ← prev[u]                           // Traverse from target to source

// Use Dijkstra's algorithm to find shortest path.
// https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
let totalHeatlossMap = Array2D.create sizeX sizeY Int32.MaxValue
let prevPoint: Point option array2d = Array2D.create sizeX sizeY None
let startingPoint = { X = 0; Y = 0; }
let endingPoint = { X = sizeX - 1; Y = sizeY - 1 }
let startingPointHeatloss = heatMap[0, 0]

// Add total heat loss for starting node
totalHeatlossMap[0, 0] <- startingPointHeatloss

let isDisallowedStraight (point1: Point) (point2: Point) (point3: Point option) (point4: Point option) (point5: Point option) =
    if (Option.isNone point3) || (Option.isNone point4) || (Option.isNone point5) then
        false
    else
        (point1.X = point2.X && point2.X = (Option.get point3).X && (Option.get point3).X = (Option.get point4).X && (Option.get point4).X = (Option.get point5).X) ||
        (point1.Y = point2.Y && point2.Y = (Option.get point3).Y && (Option.get point3).Y = (Option.get point4).Y && (Option.get point4).Y = (Option.get point5).Y)

let getNextPoints (point: Point) =
    // let endpoint = endingPointOfPath path
    let fromPoint = prevPoint[point.X, point.Y]
    let fromFromPoint = fromPoint |> Option.bind (fun x -> prevPoint[x.X, x.Y])
    let fromFromFromPoint = fromFromPoint |> Option.bind (fun x -> prevPoint[x.X, x.Y])
    let initalNextPoints = [
        { X = point.X; Y = point.Y - 1 }  // north
        { X = point.X - 1; Y = point.Y }  // west
        { X = point.X; Y = point.Y + 1 }  // south
        { X = point.X + 1; Y = point.Y }  // east
    ]
    let pointBacktracks (point: Point) = fromPoint |> Option.contains point

    initalNextPoints
        |> List.filter (
            fun point ->
                not (pointBacktracks point) &&
                point.X >= 0 && point.X < sizeX &&
                point.Y >= 0 && point.Y < sizeY &&
                not (isDisallowedStraight point point fromPoint fromFromPoint fromFromFromPoint))

let rec pickLeastStraightPath (path1: Point option list) (path2: Point option list) =
    let path1Point1 = Option.get path1[0]
    let path1Point2 = Option.get path1[1]
    let path1Point3 = path1[2]
    let path1Point4 = if List.length path1 = 4 then path1[3] else None
    let path2Point1 = Option.get path2[0]
    let path2Point2 = Option.get path2[0]
    let path2Point3 = path2[2]
    let path2Point4 = if List.length path2 = 4 then path2[3] else None

    let path1StartsVertical = path1Point1.Y = path1Point2.Y
    let path1ContinuesVertical = if Option.isSome path1Point3 then path1Point2.Y = (Option.get path1Point3).Y else false
    let path1EndsVertical = if Option.isSome path1Point3 && Option.isSome path1Point4 then (Option.get path1Point3).Y = (Option.get path1Point4).Y else false

    let path2StartsVertical = path2Point1.Y = path2Point2.Y
    let path2ContinuesVertical = if Option.isSome path2Point3 then path2Point2.Y = (Option.get path2Point3).Y else false
    let path2EndsVertical = if Option.isSome path2Point3 && Option.isSome path2Point4 then (Option.get path2Point3).Y = (Option.get path2Point4).Y else false

    let path1StraightLength =
        if path1StartsVertical then
            if path1ContinuesVertical then
                if path1EndsVertical then 4 else 3
            else 2
        else
            if not path1ContinuesVertical then
                if not path1EndsVertical then 4 else 3
            else
                2

    let path2StraightLength =
        if path2StartsVertical then
            if path2ContinuesVertical then
                if path2EndsVertical then 4 else 3
            else 2
        else
            if not path2ContinuesVertical then
                if not path2EndsVertical then 4 else 3
            else
                2

    if path1StraightLength < path2StraightLength then
        0
    elif path1StraightLength > path2StraightLength then
        1
    elif path1StraightLength = 2 && (List.length path1) = 4 then
        pickLeastStraightPath (path1 |> List.skip 1) (path2 |> List.skip 1)
    else
        1

let queue = PriorityQueue<Point, int>()

queue.Enqueue(startingPoint, startingPointHeatloss)

let getPath (point: Point) =
    let mutable currentPath = []
    let mutable point = Some point

    while Option.isSome point do
        let p = Option.get point
        currentPath <- p :: currentPath
        point <- prevPoint[p.X, p.Y]

    currentPath

let mutable isFound = false

while queue.Count > 0 && not isFound do
    let currentPoint = queue.Dequeue()

    printfn ">>> currentPoint = %A" currentPoint

    if currentPoint = endingPoint then
        isFound <- true
    else
        let currentHeatloss = totalHeatlossMap[currentPoint.X, currentPoint.Y]
        let nextPoints = getNextPoints currentPoint

        printfn "    current heatloss = %d" currentHeatloss
        printfn "    current shortest path = %A" (getPath currentPoint)
        printfn "    nextPoints = %A" nextPoints

        nextPoints |> List.iter (
            fun nextPoint ->
                let heatloss = currentHeatloss + heatMap[nextPoint.X, nextPoint.Y]

                printfn "    * comparing nextPoint heatloss (%d + %d) to totalHeatlossMap[%d, %d] (%d)..." currentHeatloss heatMap[nextPoint.X, nextPoint.Y] nextPoint.X nextPoint.Y totalHeatlossMap[nextPoint.X, nextPoint.Y]

                let shouldTake =
                    if heatloss < totalHeatlossMap[nextPoint.X, nextPoint.Y] then
                        true
                    elif heatloss = totalHeatlossMap[nextPoint.X, nextPoint.Y] then
                        let newPath =
                            let prevToCurrentPoint = prevPoint[currentPoint.X, currentPoint.Y]
                            [Some nextPoint; Some currentPoint; prevToCurrentPoint; prevToCurrentPoint |> Option.bind (fun point -> prevPoint[point.X, point.Y])]
                        let oldPath =
                            let thisPrevPoint = prevPoint[nextPoint.X, nextPoint.Y]
                            let thisPrevPrevPoint = thisPrevPoint |> Option.bind (fun point -> prevPoint[point.X, point.Y])
                            let thisPrevPrevPrevPoint = thisPrevPrevPoint |> Option.bind (fun point -> prevPoint[point.X, point.Y])
                            [Some nextPoint; thisPrevPoint; thisPrevPrevPoint; thisPrevPrevPrevPoint]
                        (pickLeastStraightPath newPath oldPath) = 0 // 0 is picking the new path
                    else
                        false

                if shouldTake then
                    totalHeatlossMap[nextPoint.X, nextPoint.Y] <- heatloss
                    prevPoint[nextPoint.X, nextPoint.Y] <- Some currentPoint

                    printfn "    prevPoint[%d, %d] = (%d, %d)" nextPoint.X nextPoint.Y currentPoint.X currentPoint.Y
                    printfn "    totalHeatlossMap[%d, %d] = %d" nextPoint.X nextPoint.Y heatloss

                    let queueContainsNextPoint = queue.UnorderedItems.Any(fun ((point, _): struct(Point * int)) -> point = nextPoint)

                    if not queueContainsNextPoint then
                        printfn "    added %A (%d)" nextPoint heatloss
                        queue.Enqueue(nextPoint, heatloss))

    printfn ""

let path = getPath endingPoint

let heatlossOfPath1 = path |> List.sumBy (fun point -> heatMap[point.X, point.Y])
let heatlossOfPath2 = totalHeatlossMap[endingPoint.X, endingPoint.Y]

printfn ""
printfn "path = %A" path
printfn ""
printfn "heatlost1 = %d" heatlossOfPath1
printfn "heatlost2 = %d" heatlossOfPath2

let isOnPath x y =
    let point = { X = x; Y = y }
    path |> List.contains point

printfn ""
draw heatMap (Some (fun x y -> if isOnPath x y then '*' else (('0' |> int) + heatMap[x, y]) |> char))
