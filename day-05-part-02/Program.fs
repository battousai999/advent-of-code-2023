open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Threading.Tasks

type MappingSpec = {
    SourceStart : int64
    DestinationStart : int64
    Length : int64
}

type CategoryMap = {
    SourceType : string
    DestinationType : string
    Mappings : MappingSpec list
}

let rawAlmanac = (File.ReadAllText("../day-05-part-01/almanac.txt")).Split(Environment.NewLine + Environment.NewLine)

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let seedsRegex = Regex(@"^seeds:\s*([\d\s]+)$", RegexOptions.IgnoreCase)
let sectionHeaderRegex = Regex(@"^(\w+)-to-(\w+)\s+map:$", RegexOptions.IgnoreCase)
let rangeSpecRegex = Regex(@"(\d+)\s(\d+)\s(\d+)")

let rawSeeds = rawAlmanac |> Array.head

let seeds =
    let seedValues =
        match rawSeeds with
        | Regexer seedsRegex [values] -> values.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map Int64.Parse |> Array.toList
        | _ -> raise <| ApplicationException("invalid input")
    let pairedValues = 
        seedValues 
            |> List.chunkBySize 2 
            |> List.map (
                fun list ->
                    match list with
                    | [start; length] -> (start, length)
                    | _ ->
                        let badInput = String.Join(", ", list)
                        raise <| ApplicationException($"invalid seed pair: ({badInput})"))
    pairedValues

let sections = 
    rawAlmanac
        |> Array.tail
        |> Array.map (
            fun section ->
                let (header, ranges) =
                    match (section.Split(Environment.NewLine) |> Array.toList) with
                    | (header :: ranges) -> (header, ranges)
                    | _ -> raise <| ApplicationException($"invalid section")
                let (source, destination) = 
                    match header with
                    | Regexer sectionHeaderRegex [source; destination] -> (source, destination)
                    | _ -> raise <| ApplicationException($"invalid header for section: {header}")
                let mappings =
                    ranges
                        |> List.map (
                            fun range ->
                                let (sourceStart, destinationStart, length) = 
                                    match range with
                                    | Regexer rangeSpecRegex [destinationStart; sourceStart; length] ->
                                        (sourceStart |> Int64.Parse, destinationStart |> Int64.Parse, length |> Int64.Parse)
                                    | _ -> raise <| ApplicationException($"invalid range ({range}) for section ({header})")
                                { SourceStart = sourceStart; DestinationStart = destinationStart; Length = length })
                { SourceType = source; DestinationType = destination; Mappings = mappings })
        |> Array.toList

let getMappingValue (mappings: MappingSpec list) (value: int64) =
    let mapping = mappings |> List.tryFind (fun x -> (value >= x.SourceStart) && (value <= (x.SourceStart + x.Length - 1L)))

    match mapping with
    | Some mapping -> mapping.DestinationStart + (value - mapping.SourceStart)
    | None -> value

let memoize f =
    let dict = Dictionary<_, _>();
    fun c ->
        let exist, value = dict.TryGetValue c
        match exist with
        | true -> value
        | _ -> 
            let value = f c
            dict.Add(c, value)
            value
let calculateLocationForSeed (seed: int64) =
    let rec applyMapping (category: string) (value: int64) =
        let section = 
            match (sections |> List.tryFind (fun x -> x.SourceType = category)) with
            | Some section -> section
            | None -> raise <| ApplicationException($"could not find section for source type: {category}")        
        let mappedValue = getMappingValue section.Mappings value

        if section.DestinationType = "location"
        then mappedValue
        else applyMapping section.DestinationType mappedValue

    applyMapping "seed" seed

let mutable minValue: int64 = Int64.MaxValue
let minValueLock = Object()

seeds 
    |> List.iter (
        fun (start, length) -> 
            Parallel.For(
                fromInclusive = start, 
                toExclusive = start + length - 1L, 
                localInit = (fun () -> Int64.MaxValue), 
                body = (fun (value: int64) (state: ParallelLoopState) (local: int64) ->
                    let result = calculateLocationForSeed value
                    Int64.Min(local, result)),
                localFinally = (fun local -> lock minValueLock (fun () -> minValue <- Int64.Min(minValue, local)))) |> ignore)

printfn "%d" minValue
