open System.IO
open System
open System.Text.RegularExpressions

type Game = {
    Hand: string
    Bid: int64
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let rawGames = File.ReadAllLines("../day-07-part-01/games.txt")

let gameRegex = Regex(@"^([AKQJT98765432]+)\s*(\d+)")

let games = 
    rawGames
        |> Array.map (
            fun line ->
                match line with
                | Regexer gameRegex [hand; bid] -> { Hand = hand; Bid = bid |> Int64.Parse }
                | _ -> raise <| ApplicationException($"invalid game: {line}"))
        |> Array.toList

let comparer (x: Game) (y: Game) =
    let handRank (game: Game) =
        let mostFrequentChar (hand: string) =
            let adjustedHand = 
                match hand.Replace("J", "") with
                | "" -> "11111"
                | x -> x
            let groupings = (adjustedHand) |> Seq.groupBy (fun x -> x)
            let (ch, _) = groupings |> Seq.maxBy (fun (_, charSeq) -> Seq.length charSeq)

            ch
        let adjustedHand = game.Hand.Replace("J", (mostFrequentChar game.Hand).ToString())

        match adjustedHand |> Seq.sort |> Seq.toList  with
        | [a; b; c; d; e] when a = b && b = c && c = d && d = e -> 7
        | [a; b; c; d; e] when (a = b && b = c && c = d) || (b = c && c = d && d = e) -> 6
        | [a; b; c; d; e] when (a = b && b = c && d = e) || (a = b && c = d && d = e) -> 5
        | [a; b; c; d; e] when (a = b && b = c) || (b = c && c = d) || (c = d && d = e) -> 4
        | [a; b; c; d; e] when (a = b && c = d) || (b = c && d = e) || (a = b && d = e) -> 3
        | [a; b; c; d; e] when (a = b) || (b = c) || (c = d) || (d = e) -> 2
        | _ -> 1
    let cardRank (a: char) =
        match a with
        | 'J' -> 0
        | '2' -> 1
        | '3' -> 2
        | '4' -> 3
        | '5' -> 4
        | '6' -> 5
        | '7' -> 6
        | '8' -> 7
        | '9' -> 8
        | 'T' -> 9
        | 'Q' -> 11
        | 'K' -> 12
        | 'A' -> 13
        | _ -> 
            raise <| ApplicationException($"invalid card: {a}")
    let rec handComparer (hand1: string) (hand2: string) =
        let aRank = cardRank (Seq.head hand1)
        let bRank = cardRank (Seq.head hand2)
        
        if aRank < bRank then -1
        elif aRank > bRank then 1
        elif hand1.Length = 1 then 0
        else 
            handComparer ((Seq.tail hand1) |> String.Concat) ((Seq.tail hand2) |> String.Concat)

    let xGameRank = handRank x
    let yGameRank = handRank y

    if xGameRank < yGameRank then -1
    elif xGameRank > yGameRank then 1
    else handComparer (x.Hand) (y.Hand)


let rankedGames =
    games
        |> List.sortWith comparer

let totalWinnings = rankedGames |> List.mapi (fun i x -> ((i |> int64) + 1L) * (x.Bid)) |> List.sum

printfn "%d" totalWinnings
