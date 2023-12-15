open System
open System.IO
open System.Text.RegularExpressions
open System.Linq

type Lense = {
    Label: string
    FocalLength: int
}

type Box = {
    BoxNumber: int
    Lenses: ResizeArray<Lense>
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let addLenseRegex = Regex(@"^(\w+)=(\d+)$")

let rawInitSequence = File.ReadAllText("../day-15-part-01/initialization.txt")

let initSequence = rawInitSequence.Split(',')

let hashValue (value: string) = value |> Seq.fold (fun (acc: int) (value: char) -> ((acc + (int value)) * 17) % 256) 0

let boxes = Array.init 256 (fun i -> { BoxNumber = i + 1; Lenses = new ResizeArray<Lense>() })

initSequence
    |> Array.iter (
        fun line ->
            match line with
            | Regexer addLenseRegex [label; focalLengthStr] ->
                let index = label |> hashValue
                let focalLength = focalLengthStr |> Int32.Parse
                let box = boxes[index]

                if box.Lenses.Any(fun x -> x.Label = label) then
                    let lenseIndex = box.Lenses.FindIndex(fun x -> x.Label = label)
                    box.Lenses[lenseIndex] <- { Label = label; FocalLength = focalLength }
                else
                    box.Lenses.Add({ Label = label; FocalLength = focalLength })
            | line when line.EndsWith('-') ->
                let label = line.Substring(0, line.Length - 1)
                let index = label |> hashValue
                let box = boxes[index]

                box.Lenses.RemoveAll(fun x -> x.Label = label) |> ignore
            | _ -> raise <| ApplicationException($"unexpected line: {line}"))

let calcFocusingPower (box: Box) =
    box.BoxNumber * (box.Lenses |> Seq.mapi (fun i x -> (i + 1) * x.FocalLength) |> Seq.sum)

let totalFocusingPower = boxes |> Array.map calcFocusingPower |> Array.sum

printfn "%d" totalFocusingPower
