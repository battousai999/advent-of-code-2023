open System
open System.IO
open System.Text.RegularExpressions

type Brick = {
    Id: int

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

let rawBricks = File.ReadAllLines("bricks.txt")

// let rawBricksStr = @"1,0,1~1,2,1
// 0,0,2~2,0,2
// 0,2,3~2,2,3
// 0,0,4~0,2,4
// 2,0,5~2,2,5
// 0,1,6~2,1,6
// 1,1,8~1,1,9"

// let rawBricks = rawBricksStr.Split(Environment.NewLine)

let bricks =
    rawBricks
        |> Array.mapi (
            fun index line ->
                match line with
                | Regexer brickRegex [x1; y1; z1; x2; y2; z2] ->
                    { Id = index; X1 = x1 |> Int32.Parse; Y1 = y1 |> Int32.Parse; Z1 = z1 |> Int32.Parse; X2 = x2 |> Int32.Parse; Y2 = y2 |> Int32.Parse; Z2 = z2 |> Int32.Parse }
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

let drawSlices (map: int option array3d) =
    [0..sizeX-1]
        |> List.iter (
            fun xIndex ->
                [sizeZ-1..-1..0]
                    |> List.iter (
                        fun zIndex ->
                            let line =
                                [0..sizeY-1]
                                    |> List.map (
                                        fun yIndex ->
                                            match map[xIndex, yIndex, zIndex] with
                                            | Some brickIndex -> (('A' |> int) + brickIndex) |> char
                                            | None -> '.')
                                    |> String.Concat
                            printfn "%s" line)
                printfn "")

// drawSlices brickMap

let brickComparer (a: Brick) (b: Brick) =
    let zIndexA = min a.Z1 a.Z2
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

let getBrickXYCells (brick: Brick) =
    let x1 = min brick.X1 brick.X2
    let x2 = max brick.X1 brick.X2
    let y1 = min brick.Y1 brick.Y2
    let y2 = max brick.Y1 brick.Y2
    [for x in x1..x2 do for y in y1..y2 -> (x, y)]

let getBrickCells (brick: Brick) =
    let x1 = min brick.X1 brick.X2
    let x2 = max brick.X1 brick.X2
    let y1 = min brick.Y1 brick.Y2
    let y2 = max brick.Y1 brick.Y2
    let z1 = min brick.Z1 brick.Z2
    let z2 = max brick.Z1 brick.Z2
    [for x in x1..x2 do for y in y1..y2 do for z in z1..z2 -> (x, y, z)]

let dropBrick (brick: Brick) =
    let rec findLowestZ (xyCells: (int * int) list) (z: int) =
        if z <= 1 then
            1
        else
            let belowCells = [for (x, y) in xyCells -> (x, y, z-1)]
            let isClearBelow = belowCells |> List.forall (fun (x, y, z) -> brickMap[x, y, z] = None || brickMap[x, y, z] = Some brick.Id)

            if isClearBelow then
                findLowestZ xyCells (z-1)
            else
                z

    let lowestZ = findLowestZ (getBrickXYCells brick) (min brick.Z1 brick.Z2)
    let zOffset = abs (brick.Z1 - brick.Z2)
    let newBrick = { brick with Z1 = lowestZ; Z2 = lowestZ + zOffset }

    brick |> getBrickCells |> List.iter (fun (x, y, z) -> brickMap[x, y, z] <- None)

    newBrick |> getBrickCells |> List.iter (fun (x, y, z) -> brickMap[x, y, z] <- Some newBrick.Id)

    bricks[brick.Id] <- newBrick

zOrderedBricks |> Array.iter dropBrick

// printfn ""
// printfn "----------------"
// printfn ""
// drawSlices brickMap

let canSafelyDisintegrateBrick (brick: Brick) =
    // Get list of spaces directly above brick
    // Get list of bricks occupying one of prior list's spaces
    // Map prior list (of bricks) to list of bricks occupying their below spaces, excepting original brick
    // Return false if any of prior list's lists are not empty; return true otherwise
    let id = brick.Id
    let zIndex = max brick.Z1 brick.Z2
    let xyCells = getBrickXYCells brick
    let aboveCells = [for (x, y) in xyCells -> (x, y, zIndex+1)]
    let aboveBricks = aboveCells |> List.choose (fun (x, y, z) -> brickMap[x, y, z]) |> List.distinct

    // printfn "aboveCells = %A" aboveCells
    // printfn "aboveBricks = %A" aboveBricks

    if aboveBricks |> List.isEmpty then
        true
    else
        let otherSupportingBricks =
            aboveBricks
                |> List.collect (
                    fun brickId ->
                        let xyCells = getBrickXYCells bricks[brickId]
                        let belowCells = [for (x, y) in xyCells -> (x, y, zIndex)]

                        belowCells
                            |> List.choose (fun (x, y, z) -> brickMap[x, y, z])
                            |> List.distinct
                            |> List.filter (fun i -> i <> id && i <> brickId))

        not (otherSupportingBricks |> List.isEmpty)

// printfn "%A" (canSafelyDisintegrateBrick bricks[2])

let numSafeBricks = bricks |> Array.toSeq |> Seq.filter canSafelyDisintegrateBrick |> Seq.length

printfn "%d" numSafeBricks
