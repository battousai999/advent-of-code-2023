open System.IO
open System

let calibrationData = File.ReadAllLines("../day-01-part-01/calibration.txt")

let rec tails (s: string) =
    match s with
    | "" -> Seq.empty
    | otherwise -> Seq.append (Seq.singleton s) (tails (s.[1..]))

let calibrations = calibrationData |> Array.map (fun line ->
    let digitProjection (x: string) =
        match x with
        | x when Char.IsDigit x[0] -> Some (x[0] |> string |> Int32.Parse)
        | x when x.ToLower().StartsWith("zero") -> Some 0
        | x when x.ToLower().StartsWith("one") -> Some 1
        | x when x.ToLower().StartsWith("two") -> Some 2
        | x when x.ToLower().StartsWith("three") -> Some 3
        | x when x.ToLower().StartsWith("four") -> Some 4
        | x when x.ToLower().StartsWith("five") -> Some 5
        | x when x.ToLower().StartsWith("six") -> Some 6
        | x when x.ToLower().StartsWith("seven") -> Some 7
        | x when x.ToLower().StartsWith("eight") -> Some 8
        | x when x.ToLower().StartsWith("nine") -> Some 9
        | _ -> None

    let firstDigit = line |> tails |> Seq.map digitProjection |> Seq.find (fun x -> x.IsSome) |> Option.get
    let lastDigit = line |> tails |> Seq.map digitProjection |> Seq.findBack (fun x -> x.IsSome) |> Option.get

    firstDigit * 10 + lastDigit)

let sum = calibrations |> Array.sum

printfn "%d" sum
