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

// let rawDigPlan = File.ReadAllLines("dig-plan.txt")

let rawDigPlanStr = @"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"

let rawDigPlan = rawDigPlanStr.Split(Environment.NewLine)

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

let startingPoint = { X = 0; Y = 0 }

let points =
    let (_, tailPoints) =
        digPlan
            |> List.fold (
                fun ((prevPoint, list): Point * ResizeArray<Point>) (instruction: Instruction) ->
                    let nextPoint =
                        match instruction.Direction with
                        | Direction.Up -> { X = prevPoint.X; Y = prevPoint.Y - (instruction.Length+1) }
                        | Direction.Down -> { X = prevPoint.X; Y = prevPoint.Y + (instruction.Length+1) }
                        | Direction.Left -> { X = prevPoint.X - (instruction.Length+1); Y = prevPoint.Y }
                        | Direction.Right -> { X = prevPoint.X + (instruction.Length+1); Y = prevPoint.Y }
                    list.Add(nextPoint)
                    (nextPoint, list))
                (startingPoint, ResizeArray<Point>())

    tailPoints |> Seq.toArray

    // let lastPoint =
    //     let point = tailPoints.[tailPoints.Count - 1]
    //     tailPoints.RemoveAt(tailPoints.Count - 1)
    //     point

    // (lastPoint :: startingPoint :: (tailPoints |> Seq.toList)) |> Array.ofList

let points2 = [|
    { X = 0; Y = 0};
    { X = 0; Y = 10};
    { X = 10; Y = 10};
    { X = 10; Y = 0};
|]

printfn "%A" points

let area =
    let size = Array.length points
    //let sum = [1..(Array.length points)-1] |> List.sumBy (fun i -> points[i].X * (points[(i+1) % size].Y - points[i-1].Y))
    let sum = [0..(Array.length points)-1] |> List.sumBy (fun i -> (points[i].X * points.[(i+1) % size].Y) - (points.[(i+1) % size].X * points.[i].Y))
    abs(sum / 2)

printfn "%d" area
