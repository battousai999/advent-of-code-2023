open System
open System.IO
open System.Text.RegularExpressions

type Point = {
    X: int64
    Y: int64
    Z: int64
}

type Velocity = {
    X: int64
    Y: int64
    Z: int64
}

type Motion = {
    Position: Point
    Velocity: Velocity
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let positionAndVelocityRegex = Regex(@"^\s*(\d+),\s*(\d+),\s*(\d+)\s*@\s*([-\d]+),\s*([-\d]+),\s*([-\d]+)$")

let rawPositionAndVelocity = File.ReadAllLines("position-and-velocity.txt")

// let rawPositionAndVelocityStr = @"19, 13, 30 @ -2,  1, -2
// 18, 19, 22 @ -1, -1, -2
// 20, 25, 34 @ -2, -2, -4
// 12, 31, 28 @ -1, -2, -1
// 20, 19, 15 @  1, -5, -3"

// let rawPositionAndVelocity = rawPositionAndVelocityStr.Split(Environment.NewLine)

let entries =
    rawPositionAndVelocity
        |> Array.map (
            fun line ->
                match line with
                | Regexer positionAndVelocityRegex [posX; posY; posZ; velX; velY; velZ] ->
                    {
                        Position = { Point.X = posX |> int64; Y = posY |> int64; Z = posZ |> int64 };
                        Velocity = { Velocity.X = velX |> int64; Y = velY |> int64; Z = velZ |> int64 }
                    }
                | _ -> raise <| ApplicationException($"invalid position/velocity specification: {line}"))

let areVelocitiesParallel (velocity1: Velocity) (velocity2: Velocity) = (velocity1.Y * velocity2.X) - (velocity1.X * velocity2.Y) = 0

let lineXYIntersection (entry1: Motion) (entry2: Motion) =
    let pos1X = entry1.Position.X |> double
    let pos1Y = entry1.Position.Y |> double
    let pos2X = entry2.Position.X |> double
    let pos2Y = entry2.Position.Y |> double
    let vel1X = entry1.Velocity.X |> double
    let vel1Y = entry1.Velocity.Y |> double
    let vel2X = entry2.Velocity.X |> double
    let vel2Y = entry2.Velocity.Y |> double

    let case1 = vel1X = 0 && vel2X <> 0
    let case2 = vel1Y = 0 && vel2Y <> 0

    if case1 || case2 then
        let factor =
            if case1 then
                (pos1X - pos2X) / vel2X
            else
                (pos1Y - pos2Y) / vel2Y
        Some (pos2X + factor * vel2X, pos2Y + factor * vel2Y)
    else
        let intersection factor = (pos1X + factor * vel1X, pos1Y + factor * vel1Y)

        if vel1X <> 0 && vel1Y <> 0 && vel2X <> 0 && vel2Y <> 0 then
            if areVelocitiesParallel entry1.Velocity entry2.Velocity then
                None
            else
                let factor = (pos1X * vel2Y - pos1Y * vel2X - pos2X * vel2Y + pos2Y * vel2X) / (vel1Y * vel2X - vel1X * vel2Y)
                Some <| intersection factor
        elif vel2X = 0 && vel1X <> 0 then
            let factor = (pos2X - pos1X) / vel1X
            Some <| intersection factor
        elif vel2Y = 0 && vel1Y <> 0 then
            let factor = (pos2Y - pos1Y) / vel1Y
            Some <| intersection factor
        else
            None

let rec combinations n list =
    match n, list with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

let isInPast (entry: Motion) ((intersectX, intersectY): double * double) =
    let isInPastX =
        if entry.Velocity.X > 0 then
            (entry.Position.X |> double) > intersectX
        else
            (entry.Position.X |> double) < intersectX
    let isInPastY =
        if entry.Velocity.Y > 0 then
            (entry.Position.Y |> double) > intersectY
        else
            (entry.Position.Y |> double) < intersectY

    isInPastX || isInPastY

let collideWithinBoundary (min: int64) (max: int64) =
    let pairings =
        combinations 2 (entries |> Array.toList)
            |> List.map (
                fun list->
                    match list with
                    | [a; b] -> (a, b)
                    | _ -> raise <| ApplicationException(sprintf "pairing wasn't a pair: %A" list))
    let intersections =
        pairings
            |> List.choose (
                fun (a, b) ->
                    match lineXYIntersection a b with
                    | Some intersection -> Some (a, b, intersection)
                    | None -> None)
    let validIntersections =
        intersections
            |> List.filter (
                fun (a, b, intersection) ->
                    let (intersectionX, intersectionY) = intersection
                    let isInBoundaryX = intersectionX >= (min |> double) && intersectionX <= (max |> double)
                    let isInBoundaryY = intersectionY >= (min |> double) && intersectionY <= (max |> double)
                    let isInPastA = isInPast a intersection
                    let isInPastB = isInPast b intersection

                    isInBoundaryX && isInBoundaryY && (not isInPastA) && (not isInPastB))

    validIntersections |> List.length

let numIntersections = collideWithinBoundary 200000000000000L 400000000000000L

printfn "%d" numIntersections
