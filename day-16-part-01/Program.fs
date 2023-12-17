open System
open System.IO

type CellType =
| Empty
| SlashMirror
| BackslashMirror
| VerticalSplitter
| HorizontalSplitter

type Direction =
| North = 0
| West = 1
| South = 2
| East = 3

type BeamType =
| NoBeam
| Directed

type Cell = {
    CellType: CellType
    Beams: BeamType array
}

let rawContraption = File.ReadAllLines("contraption.txt")

let sizeY = rawContraption |> Array.length
let sizeX = rawContraption |> Array.head |> Seq.length

let contraptionMap = Array2D.create sizeX sizeY { CellType = CellType.Empty; Beams = Array.create 4 BeamType.NoBeam }

rawContraption
    |> Array.iteri (
        fun yIndex line ->
            line
                |> Seq.iteri (
                    fun xIndex ch ->
                        let cellType =
                            match ch with
                            | '.' -> CellType.Empty
                            | '/' -> CellType.SlashMirror
                            | '\\' -> CellType.BackslashMirror
                            | '|' -> CellType.VerticalSplitter
                            | '-' -> CellType.HorizontalSplitter
                            | _ -> raise <| ApplicationException($"unexpected cell type: {ch}")
                        let cell = { CellType = cellType; Beams = Array.create 4 BeamType.NoBeam }

                        contraptionMap[xIndex, yIndex] <- cell))

let draw (map: Cell array2d) =
    seq {0..sizeY-1}
        |> Seq.iter (
            fun yIndex ->
                let line =
                    seq {0..sizeX-1}
                        |> Seq.map (
                            fun xIndex ->
                                match map[xIndex, yIndex] with
                                | { CellType = CellType.Empty; Beams = beams } ->
                                    let beamCount = beams |> Array.map (fun b -> if b = BeamType.NoBeam then 0 else 1) |> Array.sum

                                    if beamCount = 0 then
                                        '.'
                                    elif beamCount = 1 then
                                        let directionIndex = beams |> Array.findIndex (fun x -> x <> BeamType.NoBeam)
                                        match (directionIndex |> enum<Direction>) with
                                        | Direction.North -> '↑'
                                        | Direction.West -> '←'
                                        | Direction.South -> '↓'
                                        | Direction.East -> '→'
                                        | _ -> raise <| ApplicationException($"unexpected direction index: {directionIndex}")
                                    else
                                        (('0' |> int) + beamCount) |> char
                                | { CellType = CellType.SlashMirror } -> '/'
                                | { CellType = CellType.BackslashMirror } -> '\\'
                                | { CellType = CellType.VerticalSplitter } -> '|'
                                | { CellType = CellType.HorizontalSplitter } -> '-')
                let line2 = line |> String.Concat
                printfn "%s" line2)

draw contraptionMap
printfn ""

let calculateDirectedPoint (x: int) (y: int) (direction: Direction): (int * int) option =
    match direction with
    | Direction.North ->
        let newY = y - 1

        if newY < 0 then None else Some (x, newY)
    | Direction.West ->
        let newX = x - 1

        if newX < 0 then None else Some (newX, y)
    | Direction.South ->
        let newY = y + 1

        if newY >= sizeY then None else Some (x, newY)
    | Direction.East ->
        let newX = x + 1

        if newX >= sizeX then None else Some (newX, y)
    | _ -> raise <| ApplicationException (sprintf "unexpected direction: %A" direction)

// recursively keep track of beams, dropping a beam upon coming to a non-None same-directed BeamType, until there are no more tracked beams...
let rec driveBeams (beams: (int * int * Direction) list) = // (x, y, direction)
    if beams |> List.isEmpty then
        ()
    else
        let newBeams =
            beams
                |> List.collect (
                    fun ((x, y, direction): int * int * Direction) ->
                        let directionIndex = direction |> int
                        let cell = contraptionMap[x, y]
                        let beamType = cell.Beams[directionIndex]

                        if beamType = BeamType.Directed then
                            List.empty
                        else
                            cell.Beams[directionIndex] <- BeamType.Directed

                            let theseBeams =
                                match cell.CellType with
                                | CellType.Empty ->
                                    match (calculateDirectedPoint x y direction) with
                                    | Some (x, y) -> List.singleton (x, y, direction)
                                    | None -> List.empty
                                | CellType.SlashMirror ->
                                    let newDirection =
                                        match direction with
                                        | Direction.North -> Direction.East
                                        | Direction.West -> Direction.South
                                        | Direction.South -> Direction.West
                                        | Direction.East -> Direction.North
                                        | _ -> raise <| ApplicationException(sprintf "unexpected direction: %A" direction)

                                    match (calculateDirectedPoint x y newDirection) with
                                    | Some (x, y) -> List.singleton (x, y, newDirection)
                                    | None -> List.empty
                                | CellType.BackslashMirror ->
                                    let newDirection =
                                        match direction with
                                        | Direction.North -> Direction.West
                                        | Direction.West -> Direction.North
                                        | Direction.South -> Direction.East
                                        | Direction.East -> Direction.South
                                        | _ -> raise <| ApplicationException(sprintf "unexpected direction: %A" direction)

                                    match (calculateDirectedPoint x y newDirection) with
                                    | Some (x, y) -> List.singleton (x, y, newDirection)
                                    | None -> List.empty
                                | CellType.VerticalSplitter ->
                                    match direction with
                                    | Direction.North
                                    | Direction.South ->
                                        match (calculateDirectedPoint x y direction) with
                                        | Some (x, y) -> List.singleton (x, y, direction)
                                        | None -> List.empty
                                    | Direction.West
                                    | Direction.East ->
                                        let otherDirectionIndex = (directionIndex + 2) % 4
                                        let hasAlreadySplit = cell.Beams[otherDirectionIndex] = BeamType.Directed

                                        if hasAlreadySplit then
                                            List.empty
                                        else
                                            let northBeam =
                                                match (calculateDirectedPoint x y Direction.North) with
                                                | Some (x, y) -> List.singleton (x, y, Direction.North)
                                                | None -> List.empty
                                            let southBeam =
                                                match (calculateDirectedPoint x y Direction.South) with
                                                | Some (x, y) -> List.singleton (x, y, Direction.South)
                                                | None -> List.empty

                                            List.append northBeam southBeam
                                    | _ -> raise <| ApplicationException(sprintf "unexpected direction: %A" direction)
                                | CellType.HorizontalSplitter ->
                                    match direction with
                                    | Direction.West
                                    | Direction.East ->
                                        match (calculateDirectedPoint x y direction) with
                                        | Some (x, y) -> List.singleton (x, y, direction)
                                        | None -> List.empty
                                    | Direction.North
                                    | Direction.South ->
                                        let otherDirectionIndex = (directionIndex + 2) % 4
                                        let hasAlreadySplit = cell.Beams[otherDirectionIndex] = BeamType.Directed

                                        if hasAlreadySplit then
                                            List.empty
                                        else
                                            let westBeam =
                                                match (calculateDirectedPoint x y Direction.West) with
                                                | Some (x, y) -> List.singleton (x, y, Direction.West)
                                                | None -> List.empty
                                            let eastBeam =
                                                match (calculateDirectedPoint x y Direction.East) with
                                                | Some (x, y) -> List.singleton (x, y, Direction.East)
                                                | None -> List.empty

                                            List.append westBeam eastBeam
                                    | _ -> raise <| ApplicationException(sprintf "unexpected direction: %A" direction)

                            theseBeams)

        driveBeams newBeams


driveBeams [ (0, 0, Direction.East) ]

draw contraptionMap

let array2d_foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for x in 0 .. Array2D.length1 array - 1 do
        for y in 0 .. Array2D.length2 array - 1 do
            state <- folder x y state (array.[x, y])
    state

let energizedCount =
    contraptionMap
        |> Array2D.map (fun cell -> if (cell.Beams |> Array.exists (fun x -> x = BeamType.Directed)) then 1 else 0)
        |> array2d_foldi (fun _ _ acc value -> acc + value) 0

printfn ""
printfn "%d" energizedCount
