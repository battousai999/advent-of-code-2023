open System
open System.IO

type Pattern = {
    Map: string array
    TransposedMap: string array
    SizeX: int
    SizeY: int
}

let rawPatternsSetStr = File.ReadAllText("../day-13-part-01/patterns.txt")

// let rawPatternsSetStr = @"#.##..##.
// ..#.##.#.
// ##......#
// ##......#
// ..#.##.#.
// ..##..##.
// #.#.##.#.

// #...##..#
// #....#..#
// ..##..###
// #####.##.
// #####.##.
// ..##..###
// #....#..#"

let rawPatternsSet = rawPatternsSetStr.Split(Environment.NewLine + Environment.NewLine)

let patterns =
    rawPatternsSet
        |> Array.map (
            fun linesStr ->
                let lines = linesStr.Split(Environment.NewLine)
                let sizeX = lines |> Array.head |> Seq.length
                let sizeY = lines |> Array.length
                let patternMap = Array2D.init sizeX sizeY (fun x y -> lines[y][x])
                let transposedMap = Array2D.init sizeY sizeX (fun x y -> patternMap[y, x])
                let transposedLines =
                    [0..sizeX-1]
                        |> List.map (
                            fun y ->
                                let line = ([0..sizeY-1] |> List.map (fun x -> transposedMap[x, y])) |> String.Concat
                                line)
                        |> List.toArray

                { Map = lines; TransposedMap = transposedLines; SizeX = sizeX; SizeY = sizeY })

let calculateIndexOfHorizontalLineReflection (lines: string array) (originalIndex: int option) =
    let linesSize = lines |> Array.length
    let candidates =
        Array.pairwise lines
            |> Array.mapi (
                fun i ((line1, line2): string * string) ->
                    if line1 = line2 then
                        Some (i, i+1)
                    else
                        None)
            |> Array.choose (fun x -> x)
    let rec isReflection ((lineIndex1, lineIndex2): int * int) =
        let line1 = lines[lineIndex1]
        let line2 = lines[lineIndex2]

        if line1 = line2 then
            (lineIndex2 = linesSize - 1) || (lineIndex1 = 0) || isReflection (lineIndex1 - 1, lineIndex2 + 1)
        else
            false

    let candidate =
        candidates
            |> Array.filter (
                fun (index1, index2) ->
                    let skip =
                        match originalIndex with
                        | Some originalIndex when originalIndex = (index1 + 1) -> true
                        | _ -> false

                    not skip && isReflection (index1, index2))
            |> Array.tryHead

    candidate |> Option.map (fun ((index, _): int * int) -> index + 1)

let calculateIndexOfVerticalLineReflection (lines: string array) (originalIndex: int option) =
    let linesSize = lines |> Array.length
    let candidates =
        Array.pairwise lines
            |> Array.mapi (
                fun i ((line1, line2): string * string) ->
                    if line1 = line2 then
                        Some (i, i+1)
                    else
                        None)
            |> Array.choose id
    let rec isReflection ((lineIndex1, lineIndex2): int * int) =
        let line1 = lines[lineIndex1]
        let line2 = lines[lineIndex2]

        if line1 = line2 then
            (lineIndex2 = linesSize - 1) || (lineIndex1 = 0) || isReflection (lineIndex1 - 1, lineIndex2 + 1)
        else
            false

    let candidate =
        candidates
            |> Array.filter (
                fun (index1, index2) ->
                    let skip =
                        match originalIndex with
                        | Some originalIndex when originalIndex = (index1 + 1) -> true
                        | _ -> false

                    not skip && isReflection (index1, index2))
            |> Array.tryHead

    candidate |> Option.map (fun ((index, _): int * int) -> index + 1)

let calculateScoreOfAdjustedLineReflection (pattern: Pattern) =
    let originalAnswer =
        let horizontalIndex = calculateIndexOfHorizontalLineReflection pattern.Map None
        let verticalIndex = calculateIndexOfVerticalLineReflection pattern.TransposedMap None

        match (horizontalIndex, verticalIndex) with
        | (Some index, _) -> (true, index)
        | (_, Some index) -> (false, index)
        | _ -> raise <| ApplicationException(sprintf "cannot determine original answer for pattern: %A" pattern)

    let adjustedPatternMaps  =
        let flip x y (lines: string array) =
            lines
                |> Array.mapi (
                    fun yIndex line ->
                        line
                            |> Seq.mapi (
                                fun xIndex ch ->
                                    if xIndex = x && yIndex = y then
                                        if ch = '.' then '#' else '.'
                                    else
                                        ch)
                            |> String.Concat)
        let horizontalPatterns =
            [for x in 0..pattern.SizeX do for y in 0..pattern.SizeY -> (x, y)]
                |> List.map (fun (x, y) -> flip x y pattern.Map)
        let verticalPatterns =
            [for x in 0..pattern.SizeY do for y in 0..pattern.SizeX -> (x, y)]
                |> List.map (fun (x, y) -> flip x y pattern.TransposedMap)

        List.zip horizontalPatterns verticalPatterns

    let result =
        adjustedPatternMaps
            |> List.tryPick (
                fun (horizontalLines, verticalLines) ->
                    let originalIndex =
                        match originalAnswer with
                        | (true, index) -> Some index
                        | _ -> None
                    let horizontalReflectionIndex = calculateIndexOfHorizontalLineReflection horizontalLines originalIndex

                    match horizontalReflectionIndex with
                    | Some index -> Some (true, index)
                    | None ->
                        let originalIndex =
                            match originalAnswer with
                            | (false, index) -> Some index
                            | _ -> None
                        let verticalReflectionIndex = calculateIndexOfVerticalLineReflection verticalLines originalIndex

                        match verticalReflectionIndex with
                        | Some index -> Some (false, index)
                        | None -> None)

    match result with
    | Some (true, index) -> 100 * index
    | Some (false, index) -> index
    | None -> raise <| ApplicationException(sprintf "cannot find adjusted reflection for pattern: %A" pattern)

let sumOfScores = patterns |> Array.map calculateScoreOfAdjustedLineReflection |> Array.sum

printfn "%d" sumOfScores
