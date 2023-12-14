open System
open System.Text
open System.IO

type Item =
| Empty
| Rock
| Pillar

let rawPlatform = File.ReadAllLines("platform.txt")

// let rawPlatformStr = @"O....#....
// O.OO#....#
// .....##...
// OO.#O....O
// .O.....O#.
// O.#..O.#.#
// ..O..#O..O
// .......O..
// #....###..
// #OO..#...."

// let rawPlatform = rawPlatformStr.Split(Environment.NewLine)

let platform =
    let width = (rawPlatform |> Array.head).Length
    let builders = [|0..width-1|] |> Array.map (fun _ -> StringBuilder())

    rawPlatform
        |> Array.iter (fun line -> line |> Seq.iteri (fun i ch -> builders[i].Append(ch) |> ignore))

    builders |> Array.map (fun builder -> builder.ToString())

let slideComparer (ch1: char) (ch2: char) =
    if ch1 = ch2 then
        0
    elif ch1 = 'O' then
        -1
    else
        1

let tiltedPlatform =
    platform
        |> Array.map (
            fun line ->
                let splitSections = line.Split('#')
                let slidSections = splitSections |> Array.map (fun x -> x |> Seq.sortWith slideComparer |> String.Concat)

                String.Join('#', slidSections))

let totalLoad =
    let width = (tiltedPlatform |> Array.head).Length
    tiltedPlatform
        |> Array.map (
            fun line ->
                line |> Seq.mapi (fun i ch -> (width - i) * (if ch = 'O' then 1 else 0) |> int64) |> Seq.sum)
        |> Array.sum

printfn "%d" totalLoad
