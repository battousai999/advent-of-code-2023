open System
open System.IO
open System.Text.RegularExpressions

type Direction =
| Up
| Down
| Left
| Right

type MapCellType =
| Empty
| Trench
| InnerTrench

type Point = {
    X: int
    Y: int
}

type Instruction = {
    Direction: Direction
    Length: int
    RgbCode: string
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let instructionRegex = Regex(@"^(\w)\s(\d+)\s\(#([\da-f]{6})\)$")

let rawDigPlan = File.ReadAllLines("dig-plan.txt")

// let rawDigPlanStr = @"R 6 (#70c710)
// D 5 (#0dc571)
// L 2 (#5713f0)
// D 2 (#d2c081)
// R 2 (#59c680)
// D 2 (#411b91)
// L 5 (#8ceee2)
// U 2 (#caa173)
// L 1 (#1b58a2)
// U 2 (#caa171)
// R 2 (#7807d2)
// U 3 (#a77fa3)
// L 2 (#015232)
// U 2 (#7a21e3)"

// let rawDigPlan = rawDigPlanStr.Split(Environment.NewLine)

let digPlan =
    rawDigPlan
        |> Array.map (
            fun line ->
                match line with
                | Regexer instructionRegex [directionStr; length; rgbCode] ->
                    let direction =
                        match directionStr with
                        | "U" -> Direction.Up
                        | "D" -> Direction.Down
                        | "L" -> Direction.Left
                        | "R" -> Direction.Right
                        | _ -> raise <| ApplicationException($"unexpected direction: {directionStr}")
                    { Direction = direction; Length = length |> Int32.Parse; RgbCode = rgbCode }
                | _ -> raise <| ApplicationException($"line did not match instruction format: {line}"))
        |> Array.toList

let (_, xMin, xMax) =
    digPlan
        |> List.filter (fun x -> x.Direction = Direction.Left || x.Direction = Direction.Right)
        |> List.fold (
            fun ((offset, min, max): int * int * int) instruction ->
                match instruction.Direction with
                | Direction.Left -> (offset - instruction.Length, Int32.Min(min, offset - instruction.Length), max)
                | Direction.Right -> (offset + instruction.Length, min, Int32.Max(max, offset + instruction.Length))
                | _ -> raise <| ApplicationException(sprintf "unexpected direction: %A" instruction.Direction))
            (0, 0, 0)

let (_, yMin, yMax) =
    digPlan
        |> List.filter (fun x -> x.Direction = Direction.Up || x.Direction = Direction.Down)
        |> List.fold (
            fun ((offset, min, max): int * int * int) instruction ->
                match instruction.Direction with
                | Direction.Up -> (offset - instruction.Length, Int32.Min(min, offset - instruction.Length), max)
                | Direction.Down -> (offset + instruction.Length, min, Int32.Max(max, offset + instruction.Length))
                | _ -> raise <| ApplicationException(sprintf "unexpected direction: %A" instruction.Direction))
            (0, 0, 0)

let startingPoint = { X = -xMin; Y = -yMin }

printfn "startingX = %d, minX = %d, maxX = %d" startingPoint.X xMin xMax
printfn "startingY = %d, minY = %d, maxY = %d" startingPoint.Y yMin yMax

let sizeX = xMax - xMin + 1
let sizeY = yMax - yMin + 1

let digMap = Array2D.create sizeX sizeY MapCellType.Empty

let rec dig (digPlan: Instruction list) (currentPosition: Point) =
    match digPlan with
    | [] -> ()
    | instruction :: remainingDigPlan ->
        let cellList =
            match instruction.Direction with
            | Direction.Up -> [1..instruction.Length] |> List.map (fun yOffset -> { X = currentPosition.X; Y = currentPosition.Y - yOffset })
            | Direction.Down -> [1..instruction.Length] |> List.map (fun yOffset -> { X = currentPosition.X; Y = currentPosition.Y + yOffset })
            | Direction.Left -> [1..instruction.Length] |> List.map (fun xOffset -> { X = currentPosition.X - xOffset; Y = currentPosition.Y })
            | Direction.Right -> [1..instruction.Length] |> List.map (fun xOffset -> { X = currentPosition.X + xOffset; Y = currentPosition.Y })

        cellList |> List.iter (fun point -> digMap[point.X, point.Y] <- MapCellType.Trench)
        dig remainingDigPlan (cellList |> List.last)

dig digPlan startingPoint

let draw (map: MapCellType array2d) =
    [0..sizeY-1]
        |> List.iter (
            fun yIndex ->
                let line =
                    [0..sizeX-1]
                        |> List.map (
                            fun xIndex ->
                                match map[xIndex, yIndex] with
                                | MapCellType.Empty -> '.'
                                | MapCellType.Trench -> if xIndex = startingPoint.X && yIndex = startingPoint.Y then 'S' else '#'
                                | MapCellType.InnerTrench -> '•')
                        |> String.Concat

                printfn "%s" line)

let followPathMarkingRight (map: MapCellType array2d) (startingPoint: Point) =
    let findNeighboringPathPoints (point: Point) =
        let neighbors = [
            { X = point.X; Y = point.Y - 1 };
            { X = point.X - 1; Y = point.Y };
            { X = point.X + 1; Y = point.Y };
            { X = point.X; Y = point.Y + 1 };
        ]
        let validNeighbors = neighbors |> List.filter (fun point -> point.X >=0 && point.X < sizeX && point.Y >= 0 && point.Y < sizeY)
        validNeighbors |> List.filter (fun point -> map[point.X, point.Y] = MapCellType.Trench)

    let rec followPath (currentPoint: Point) (prevPoint: Point) (direction: Direction) =
        if currentPoint = startingPoint then
            ()
        else
            let nextPoints = (findNeighboringPathPoints currentPoint) |> List.filter (fun point -> point <> prevPoint)

            match nextPoints with
            | [nextPoint] ->
                let isAbove = nextPoint.Y = currentPoint.Y - 1
                let isBelow = nextPoint.Y = currentPoint.Y + 1
                let isToLeft = nextPoint.X = currentPoint.X - 1
                let isToRight = nextPoint.X = currentPoint.X + 1
                let fillPoints =
                    match direction with
                    | Direction.Right when isToRight -> [{ X = currentPoint.X; Y = currentPoint.Y + 1 }]
                    | Direction.Right when isAbove ->
                        [
                            { X = currentPoint.X; Y = currentPoint.Y + 1 };
                            { X = currentPoint.X + 1; Y = currentPoint.Y + 1};
                            { X = currentPoint.X + 1; Y = currentPoint.Y }
                        ]
                    | Direction.Up when isAbove -> [{ X = currentPoint.X + 1; Y = currentPoint.Y }]
                    | Direction.Up when isToLeft ->
                        [
                            { X = currentPoint.X + 1; Y = currentPoint.Y };
                            { X = currentPoint.X + 1; Y = currentPoint.Y - 1};
                            { X = currentPoint.X; Y = currentPoint.Y - 1 }
                        ]
                    | Direction.Left when isToLeft -> [{ X = currentPoint.X; Y = currentPoint.Y - 1 }]
                    | Direction.Left when isBelow ->
                        [
                            { X = currentPoint.X; Y = currentPoint.Y - 1 };
                            { X = currentPoint.X - 1; Y = currentPoint.Y - 1 };
                            { X = currentPoint.X - 1; Y = currentPoint.Y }
                        ]
                    | Direction.Down when isBelow -> [{ X = currentPoint.X - 1; Y = currentPoint.Y }]
                    | Direction.Down when isToRight ->
                        [
                            { X = currentPoint.X - 1; Y = currentPoint.Y };
                            { X = currentPoint.X - 1; Y = currentPoint.Y + 1};
                            { X = currentPoint.X; Y = currentPoint.Y + 1 }
                        ]
                    | _ -> []

                fillPoints
                    |> List.filter (fun point -> map[point.X, point.Y] = MapCellType.Empty)
                    |> List.iter (fun point -> map[point.X, point.Y] <- MapCellType.InnerTrench)

                let newDirection =
                    if isAbove then
                        Direction.Up
                    elif isBelow then
                        Direction.Down
                    elif isToLeft then
                        Direction.Left
                    else
                        Direction.Right

                followPath nextPoint currentPoint newDirection
            | _ -> raise <| ApplicationException(sprintf "invalid next points (should be a singleton): %A" nextPoints)

    followPath { X = startingPoint.X + 1; Y = startingPoint.Y } startingPoint Direction.Right

followPathMarkingRight digMap startingPoint

let markAdjacentToInnerTrench (map: MapCellType array2d) =
    let findNeighboringPathPoints (point: Point) =
        let neighbors = [
            { X = point.X; Y = point.Y - 1 };
            { X = point.X - 1; Y = point.Y };
            { X = point.X + 1; Y = point.Y };
            { X = point.X; Y = point.Y + 1 };
        ]
        let validNeighbors = neighbors |> List.filter (fun point -> point.X >=0 && point.X < sizeX && point.Y >= 0 && point.Y < sizeY)
        validNeighbors |> List.filter (fun point -> map[point.X, point.Y] = MapCellType.Empty)

    let fillAdjacent (point: Point) =
        let validNeighbors = findNeighboringPathPoints point
        validNeighbors |> List.iter (fun point -> map[point.X, point.Y] <- MapCellType.InnerTrench)

    (seq { for x in 0..sizeX-1 do for y in 0..sizeY-1 -> { X = x; Y = y } })
        |> Seq.filter (fun point -> map[point.X, point.Y] = MapCellType.InnerTrench)
        |> Seq.iter fillAdjacent

markAdjacentToInnerTrench digMap

draw digMap

let totalTrenchArea =
    (seq { for x in 0..sizeX-1 do for y in 0..sizeY-1 -> { X = x; Y = y } })
        |> Seq.filter (
            fun point ->
                let cellType = digMap[point.X, point.Y]
                cellType = MapCellType.InnerTrench || cellType = MapCellType.Trench)
        |> Seq.length

printfn ""
printfn "area = %d" totalTrenchArea
