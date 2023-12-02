open System.Text.RegularExpressions
open System.IO
open System

type Sample = {
    Red : int
    Green : int
    Blue : int
}

type Game = {
    Id : int
    Samples : Sample list
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let gameRegex = Regex(@"^Game\s(\d+):\s(.*)$", RegexOptions.IgnoreCase)
let redRegex = Regex(@"(\d+) red", RegexOptions.IgnoreCase)
let greenRegex = Regex(@"(\d+) green", RegexOptions.IgnoreCase)
let blueRegex = Regex(@"(\d+) blue", RegexOptions.IgnoreCase)

let rawGames = File.ReadAllLines("../day-02-part-01/samples.txt")

let games = rawGames |> Array.map (
    fun line -> 
        match line with
        | Regexer gameRegex [id; samplesText] ->
            let rawSamples = samplesText.Split(";")
            let samples = 
                rawSamples 
                    |> Array.map (
                        fun rawSample ->
                            let numRed = 
                                match rawSample with
                                | Regexer redRegex [num] -> num |> Int32.Parse
                                | _ -> 0
                            let numGreen = 
                                match rawSample with
                                | Regexer greenRegex [num] -> num |> Int32.Parse
                                | _ -> 0
                            let numBlue = 
                                match rawSample with
                                | Regexer blueRegex [num] -> num |> Int32.Parse
                                | _ -> 0
                            
                            { Red = numRed; Green = numGreen; Blue = numBlue })
                    |> Array.toList
            
            { Id = id |> Int32.Parse; Samples = samples}
        | _ -> raise <| ApplicationException "Invalid game")

let gameConstraints = 
    games 
        |> Array.map (
            fun game ->
                let numRed = game.Samples |> List.map (fun x -> x.Red) |> List.max
                let numGreen = game.Samples |> List.map (fun x -> x.Green) |> List.max
                let numBlue = game.Samples |> List.map (fun x -> x.Blue) |> List.max

                { Red = numRed; Green = numGreen; Blue = numBlue })

let totalPower =
    gameConstraints
        |> Array.map (fun sample -> sample.Red * sample.Green * sample.Blue)
        |> Array.sum

printfn "Total power = %d" totalPower
