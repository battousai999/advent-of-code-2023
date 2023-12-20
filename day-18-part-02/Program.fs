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
    X: int64
    Y: int64
}

type Instruction = {
    Direction: Direction
    Length: int64
    RgbCode: string
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let instructionRegex = Regex(@"^(\w)\s(\d+)\s\(#([\da-f]{6})\)$")

let rawDigPlan = File.ReadAllLines("../day-18-part-01/dig-plan.txt")

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

let initialDigPlan =
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
                    { Direction = direction; Length = length |> Int64.Parse; RgbCode = rgbCode }
                | _ -> raise <| ApplicationException($"line did not match instruction format: {line}"))
        |> Array.toList

let digPlan =
    initialDigPlan
        |> List.map (
            fun instruction ->
                let rawLength = instruction.RgbCode[0..4]
                let rawDirection = instruction.RgbCode.[5]
                let length = Convert.ToInt32(rawLength, 16)
                let direction =
                    match rawDirection with
                    | '0' -> Direction.Right
                    | '1' -> Direction.Down
                    | '2' -> Direction.Left
                    | '3' -> Direction.Up
                    | _ -> raise <| ApplicationException($"unexpected direction: {rawDirection}")

                { Direction = direction; Length = length; RgbCode = instruction.RgbCode })

let startingPoint = { X = 0; Y = 0 }

let points =
    let (_, tailPoints) =
        digPlan
            |> List.fold (
                fun ((prevPoint, list): Point * ResizeArray<Point>) (instruction: Instruction) ->
                    let nextPoint =
                        match instruction.Direction with
                        | Direction.Up -> { X = prevPoint.X; Y = prevPoint.Y - instruction.Length }
                        | Direction.Down -> { X = prevPoint.X; Y = prevPoint.Y + instruction.Length }
                        | Direction.Left -> { X = prevPoint.X - instruction.Length; Y = prevPoint.Y }
                        | Direction.Right -> { X = prevPoint.X + instruction.Length; Y = prevPoint.Y }
                    list.Add(nextPoint)
                    (nextPoint, list))
                (startingPoint, ResizeArray<Point>())

    tailPoints |> Seq.toArray

    // let lastPoint =
    //     let point = tailPoints.[tailPoints.Count - 1]
    //     tailPoints.RemoveAt(tailPoints.Count - 1)
    //     point

    // (lastPoint :: startingPoint :: (tailPoints |> Seq.toList)) |> Array.ofList

// let points2 = [|
//     { X = 0; Y = 0};
//     { X = 0; Y = 10};
//     { X = 10; Y = 10};
//     { X = 10; Y = 0};
// |]

// printfn "%A" points

let area =
    let size = Array.length points
    //let sum = [1..(Array.length points)-1] |> List.sumBy (fun i -> points[i].X * (points[(i+1) % size].Y - points[i-1].Y))
    let sum = [0..(Array.length points)-1] |> List.sumBy (fun i -> (points[i].X * points.[(i+1) % size].Y) - (points.[(i+1) % size].X * points.[i].Y))
    let perimeter = digPlan |> List.sumBy (fun instruction -> instruction.Length)

    printfn "%d" sum
    printfn "%d" perimeter

    abs(sum / 2L) + (perimeter / 2L) + 1L

printfn "%d" area
