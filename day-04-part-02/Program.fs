open System
open System.IO
open System.Text.RegularExpressions

type ScratchCard = {
    CardNumber : int
    WinningNumbers : int list
    ContestNumbers : int list
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let rawScratchCards = File.ReadAllLines("../day-04-part-01/scratchcards.txt")

let cardRegex = Regex(@"^Card\s+(\d+): ([\d\s]+) \| ([\d\s]+)$", RegexOptions.IgnoreCase)

let scratchCards = 
    rawScratchCards
        |> Array.map (
            fun line -> 
                match line with
                | Regexer cardRegex [cardNumber; winningNumbersStr; contestNumbersStr] ->
                    let winningNumbers = winningNumbersStr.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse |> Array.toList
                    let contestNumbers = contestNumbersStr.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse |> Array.toList

                    { CardNumber = cardNumber |> Int32.Parse; WinningNumbers = winningNumbers; ContestNumbers = contestNumbers }
                | _ -> raise <| ApplicationException("Invalid line in input: " + line))
        |> Array.toList

let initialBuckets = scratchCards |> List.map (fun card -> 1) |> List.toArray

let scratchCardFolder ((index, counters) : int * int array) (card : ScratchCard) =
    let numWinners = card.ContestNumbers |> List.where (fun x -> card.WinningNumbers |> List.contains x) |> List.length

    [1..numWinners] |> List.iter (fun index -> counters[(card.CardNumber - 1) + index] <- counters[(card.CardNumber - 1) + index] + counters[card.CardNumber - 1])

    (index + 1, counters)

let (_, processedScratchCards) =
    scratchCards
        |> List.fold scratchCardFolder (0, initialBuckets)

let sumOfPoints = processedScratchCards |> Array.sum

printfn "%d" sumOfPoints
