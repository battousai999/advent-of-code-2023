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

let rawScratchCards = File.ReadAllLines("scratchcards.txt")

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

let scratchCardPoints =
    scratchCards
        |> List.map (
            fun scratchCard -> 
                let numWinners = scratchCard.ContestNumbers |> List.where (fun x -> scratchCard.WinningNumbers |> List.contains x) |> List.length

                if numWinners = 0 then 0 else pown 2 (numWinners - 1))

let sumOfPoints = scratchCardPoints |> List.sum

printfn "%d" sumOfPoints
