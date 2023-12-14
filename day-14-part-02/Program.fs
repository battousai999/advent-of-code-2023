open System
open System.Text
open System.IO
open System.Diagnostics

type Item =
| Empty
| Rock
| Pillar

let rawPlatform = File.ReadAllLines("../day-14-part-01/platform.txt")

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

let tiltPlatform (platform: string array) =
    platform
        |> Array.map (
            fun line ->
                let splitSections = line.Split('#')
                let slidSections = splitSections |> Array.map (fun x -> x |> Seq.sortWith slideComparer |> String.Concat)

                String.Join('#', slidSections))

let rotatePlatform (platform: string array) =
    let width = (platform |> Array.head).Length
    let builders = [|0..width-1|] |> Array.map (fun _ -> StringBuilder())

    platform
        |> Array.iter (
            fun line ->
                let reversedLine = line |> Seq.rev |> String.Concat
                reversedLine |> Seq.iteri (fun i ch -> builders[i].Append(ch) |> ignore))

    builders |> Array.map (fun builder -> builder.ToString())


// printfn "%A" (platform |> rotatePlatform)

let cyclePlatform (platform: string array) =
    platform
        |> tiltPlatform |> rotatePlatform   // tilted north, rotate west
        |> tiltPlatform |> rotatePlatform   // tilted west, rotate south
        |> tiltPlatform |> rotatePlatform   // tilted south, rotate east
        |> tiltPlatform |> rotatePlatform   // tilted east, rotate back north

let repeatCyclePlatform (numCycles: int) (platform: string array) =
    let cycleSeq = seq { for i in 0..numCycles-1 -> i }

    cycleSeq
        |> Seq.fold (
            fun (platform: string array) (i: int) ->
                if i % 1000000 = 0 then
                    printfn "cycle #%d" (i + 1)

                platform |> cyclePlatform)
            platform

let sw = Stopwatch()

sw.Start()

let finalPlatform = platform |> repeatCyclePlatform 1000000000
printfn "%A" finalPlatform

sw.Stop()

printfn "time = %s" (sw.Elapsed.ToString())

let totalLoad (platform: string array) =
    let width = (platform |> Array.head).Length
    platform
        |> Array.map (
            fun line ->
                line |> Seq.mapi (fun i ch -> (width - i) * (if ch = 'O' then 1 else 0) |> int64) |> Seq.sum)
        |> Array.sum

printfn "%d" (finalPlatform |> totalLoad)
