open System
open System.IO
open System.Text.RegularExpressions

type Brick = {
    X1: int
    Y1: int
    Z1: int

    X2: int
    Y2: int
    Z2: int
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let brickRegex = Regex(@"^(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)$")

// let rawBricks = File.ReadAllLines("bricks.txt")

let rawBricksStr = @"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9"

let rawBricks = rawBricksStr.Split(Environment.NewLine)

let bricks =
    rawBricks
        |> Array.map (
            fun line ->
                match line with
                | Regexer brickRegex [x1; y1; z1; x2; y2; z2] ->
                    { X1 = x1 |> Int32.Parse; Y1 = y1 |> Int32.Parse; Z1 = z1 |> Int32.Parse; X2 = x2 |> Int32.Parse; Y2 = y2 |> Int32.Parse; Z2 = z2 |> Int32.Parse }
                | _ -> raise <| ApplicationException($"invalid brick string: {line}"))

let getBrickDimension (brick: Brick) =
    [(brick.X1, brick.X2); (brick.Y1, brick.Y2); (brick.Z1, brick.Z2)]
        |> List.map (fun (a, b) -> if a <> b then 1 else 0)
        |> List.sum

// let brickGroupings =
//     bricks
//         |> Array.map getBrickDimension
//         |> Array.groupBy id
//         |> Array.map (fun (dimension, list) -> $"dim = {dimension}: count = {list |> Array.length}")

// brickGroupings |> Array.iter (fun x -> printfn "%s" x)

let sizeX = (bricks |> Array.map (fun brick -> max brick.X1 brick.X2) |> Array.max) + 1
let sizeY = (bricks |> Array.map (fun brick -> max brick.Y1 brick.Y2) |> Array.max) + 1
let sizeZ = (bricks |> Array.map (fun brick -> max brick.Z1 brick.Z2) |> Array.max) + 1

// printfn "sizeX = %d" sizeX
// printfn "sizeY = %d" sizeY
// printfn "sizeZ = %d" sizeZ

// let numXdimensionBricks = bricks |> Array.filter (fun brick -> brick.X1 <> brick.X2) |> Array.length
// let numYdimensionBricks = bricks |> Array.filter (fun brick -> brick.Y1 <> brick.Y2) |> Array.length
// let numZdimensionBricks = bricks |> Array.filter (fun brick -> brick.Z1 <> brick.Z2) |> Array.length

// printfn "num x-dimensional bricks = %d" numXdimensionBricks
// printfn "num y-dimensional bricks = %d" numYdimensionBricks
// printfn "num z-dimensional bricks = %d" numZdimensionBricks

let brickMap: int option array3d = Array3D.create sizeX sizeY sizeZ None

bricks
    |> Array.iteri (
        fun index brick ->
            let dimension = getBrickDimension brick

            if dimension = 0 then
                brickMap[brick.X1, brick.Y1, brick.Z1] <- Some index
            else
                if brick.X1 <> brick.X2 then
                    seq { for x in brick.X1..brick.X2 -> x } |> Seq.iter (fun x -> brickMap[x, brick.Y1, brick.Z1] <- Some index)
                elif brick.Y1 <> brick.Y2 then
                    seq { for y in brick.Y1..brick.Y2 -> y } |> Seq.iter (fun y -> brickMap[brick.X1, y, brick.Z1] <- Some index)
                elif brick.Z1 <> brick.Z2 then
                    seq { for z in brick.Z1..brick.Z2 -> z } |> Seq.iter (fun z -> brickMap[brick.X1, brick.Y1, z] <- Some index)
                else
                    raise <| ApplicationException(sprintf "unexpected 1-dimension brick: %A" brick))

// let drawSlices (map: int option array3d) =
//     [0..sizeX-1]
//         |> List.iter (
//             fun xIndex ->
//                 [sizeZ-1..-1..0]
//                     |> List.iter (
//                         fun zIndex ->
//                             let line =
//                                 [0..sizeY-1]
//                                     |> List.map (
//                                         fun yIndex ->
//                                             match map[xIndex, yIndex, zIndex] with
//                                             | Some brickIndex -> (('A' |> int) + brickIndex) |> char
//                                             | None -> '.')
//                                     |> String.Concat
//                             printfn "%s" line)
//                 printfn "")

// drawSlices brickMap

let brickComparer (a: Brick) (b: Brick) =
    let zIndexA = min a.Z1 b.Z2
    let zIndexB = min b.Z1 b.Z2

    if zIndexA < zIndexB then
        -1
    elif zIndexA > zIndexB then
        1
    else
        let aIndex = Array.IndexOf(bricks, a)
        let bIndex = Array.IndexOf(bricks, b)

        aIndex - bIndex

let zOrderedBricks = bricks |> Array.sortWith brickComparer

// printfn "%A" zOrderedBricks

let dropBrick (brick: Brick) =
    // Look at lower z-ordered bricks (in relation to brick)
    // Check whether all spaces below brick are unoccupied
    // If so, then drop 1 z-index, and recurse (i.e., try to drop the same brick again)
    // If not, then done
