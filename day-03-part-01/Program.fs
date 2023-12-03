open System
open System.IO

let schematic = File.ReadAllLines("schematic.txt") |> Array.toList

type Part = {
    PartNumber : int
    X : int
    Y : int
}

type Symbol = {
    Value : char
    X : int
    Y : int
}

let parts = 
    schematic
        |> List.mapi (
            fun (yIndex : int) (line : string) ->
                let lineFolder ((xIndex, currentPart, parts) : int * Part option * Part list) (value : char) =
                    match (value, currentPart) with
                    | (digit, None) when (Char.IsDigit digit) ->
                        (xIndex + 1, Some { PartNumber = (digit |> string |> Int32.Parse); X = xIndex; Y = yIndex }, parts)
                    | (digit, Some { PartNumber = partNum; X = x; Y = y}) when (Char.IsDigit digit) ->
                        (xIndex + 1, Some { PartNumber = (partNum * 10) + (digit |> string |> Int32.Parse); X = x; Y = y }, parts)
                    | (_, None) ->
                        (xIndex + 1, None, parts)
                    | (_, Some part) ->
                        (xIndex + 1, None, part :: parts)

                let results : int * Part option * Part list = line |> Seq.fold lineFolder (0, None, [])

                match results with
                | (_, None, parts) -> parts
                | (_, Some part, parts) -> part :: parts)
        |> List.concat

let symbols =
    schematic
        |> List.mapi (
            fun (yIndex : int) (line : string) ->
                line 
                    |> Seq.mapi (
                        fun (xIndex : int) (value : char) ->
                            match value with
                            | symbol when not (Char.IsDigit value) && (value <> '.') -> Some { Value = symbol; X = xIndex; Y = yIndex }
                            | _ -> None)
                    |> Seq.choose (fun x -> x)
                    |> Seq.toList)
        |> List.concat

let validParts =
    parts
        |> List.where (
            fun part ->
                symbols |> List.exists (fun symbol -> symbol.Y >= (part.Y - 1) && symbol.Y <= (part.Y + 1) && symbol.X >= (part.X - 1) && symbol.X <= (part.X + (part.PartNumber |> string).Length)))

let sumOfParts = validParts |> List.sumBy (fun x -> x.PartNumber)

printfn "%d" sumOfParts
