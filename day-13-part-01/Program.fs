open System
open System.IO

type Pattern = {
    Map: string array
    TransposedMap: string array
    SizeX: int
    SizeY: int
}

let stuff = Array2D.init 1 2 (fun _ _ -> 1)

let rawPatternsSetStr = File.ReadAllText("patterns.txt")

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

let calculateIndexOfHorizontalLineReflection (pattern: Pattern) =
    let lines = pattern.Map
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

    let candidate = candidates |> Array.filter isReflection |> Array.tryHead

    candidate |> Option.map (fun ((index, _): int * int) -> index + 1)

let calculateIndexOfVerticalLineReflection (pattern: Pattern) =
    let lines = pattern.TransposedMap
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

    let candidate = candidates |> Array.filter isReflection |> Array.tryHead

    candidate |> Option.map (fun ((index, _): int * int) -> index + 1)

let horizontalSum =
    patterns
        |> Array.map calculateIndexOfHorizontalLineReflection
        |> Array.choose id
        |> Array.sum

let verticalSum =
    patterns
        |> Array.map calculateIndexOfVerticalLineReflection
        |> Array.choose id
        |> Array.sum

let result = verticalSum + (100 * horizontalSum)

printfn "%d" result
