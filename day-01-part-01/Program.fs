open System.IO
open System

let calibationData = File.ReadAllLines("calibration.txt")

let calibrations = calibationData |> Array.map (fun line ->
    let firstDigit = line |> Seq.find (fun x -> Char.IsDigit x) |> string |> Int32.Parse
    let lastDigit = line |> Seq.findBack (fun x -> Char.IsDigit x) |> string |> Int32.Parse

    firstDigit * 10 + lastDigit)

let sum = calibrations |> Array.sum

printfn "%d" sum
