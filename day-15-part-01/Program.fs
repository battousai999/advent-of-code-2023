open System
open System.IO

let rawInitSequence = File.ReadAllText("initialization.txt")

let initSequence = rawInitSequence.Split(',')

let hashValue (value: string) = value |> Seq.fold (fun (acc: int) (value: char) -> ((acc + (int value)) * 17) % 256) 0

let sumOfHashes = initSequence |> Array.map hashValue |> Array.sum

printfn "%d" sumOfHashes
