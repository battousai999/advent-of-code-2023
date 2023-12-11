open System
open System.IO

type Galaxy = {
    Id: int
    X: int64
    Y: int64
}

let rawGalaxyMap = File.ReadAllLines("../day-11-part-01/galaxyMap.txt")

let emptyRowIndices =
    rawGalaxyMap
        |> Array.mapi (
            fun rowIndex line ->
                (rowIndex, line |> Seq.forall (fun ch -> ch = '.')))
        |> Array.filter (fun (rowIndex, isEmpty) -> isEmpty)
        |> Array.map (fun (rowIndex, _) -> rowIndex)
        |> Array.toList

let emptyColumnIndices =
    [0..rawGalaxyMap[0].Length-1]
        |> List.map (
            fun colIndex ->
                let isEmpty = rawGalaxyMap |> Array.forall (fun line -> line[colIndex] = '.')
                (colIndex, isEmpty))
        |> List.filter (fun (rowIndex, isEmpty) -> isEmpty)
        |> List.map (fun (colIndex, _) -> colIndex)

let (_, _, galaxies) =
    rawGalaxyMap
        |> Array.fold (
            fun ((yIndex, index, currentGalaxies): int64 * int * Galaxy list) line ->
                let lineGalaxies =
                    line
                        |> Seq.mapi (fun xIndex ch -> (xIndex |> int64, yIndex, ch = '#'))
                        |> Seq.choose (fun (xIndex, yIndex, isGalaxy) -> if isGalaxy then Some (xIndex, yIndex) else None)
                        |> Seq.mapi (fun i (xIndex, yIndex) -> { Id = index + i; X = xIndex; Y = yIndex })
                        |> Seq.toList

                (yIndex + 1L, index + (lineGalaxies |> List.length), lineGalaxies @ currentGalaxies))
            (0L, 1, [])

let expansionFactor = 1000000L

let expandedGalaxies =
    galaxies
        |> List.map (
            fun galaxy ->
                let xShift = (emptyColumnIndices |> List.filter (fun xIndex -> (xIndex |> int64) < galaxy.X) |> List.length |> int64) * (expansionFactor - 1L)
                let yShift = (emptyRowIndices |> List.filter (fun yIndex -> (yIndex |> int64) < galaxy.Y) |> List.length |> int64) * (expansionFactor - 1L)

                if xShift > 0L || yShift > 0L then
                    { galaxy with X = galaxy.X + xShift; Y = galaxy.Y + yShift }
                else
                    galaxy)

let rec pairs list = seq {
    match list with
    | x::xs -> for element in xs do yield x, element
               yield! pairs xs
    | _ -> () }

let galaxyPairings = expandedGalaxies |> pairs |> Seq.toList

let distance ((galaxy1, galaxy2): Galaxy * Galaxy) = abs (galaxy1.X - galaxy2.X) + abs (galaxy1.Y - galaxy2.Y)

let totalDistance = galaxyPairings |> List.map distance |> List.sum

printfn "%d" totalDistance

