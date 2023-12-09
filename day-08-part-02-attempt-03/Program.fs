// This was a third attempt a the problem where I stopped trying to brute force it, and instead looked at the cycles in the
// graph (which HAD to be there for it to be able to go for so many steps in my previous attempts). Lining up the "end nodes"
// for the cycle of each "starting node" (of which there are 6) will likely lead to the correct answer.

open System
open System.IO
open System.Text.RegularExpressions

type Direction =
| Left
| Right

type Node = {
    Id: string
    LeftId: string
    RightId: string
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let nodeRegex = Regex(@"(\w{3})\s=\s\((\w{3}),\s(\w{3})\)")

let rawDocuments = File.ReadAllLines("../day-08-part-01/documents.txt")

// let rawDocuments2 = @"LR

// 11A = (11B, XXX)
// 11B = (XXX, 11Z)
// 11Z = (11B, XXX)
// 22A = (22B, XXX)
// 22B = (22C, 22C)
// 22C = (22Z, 22Z)
// 22Z = (22B, 22B)
// XXX = (XXX, XXX)"

// let rawDocuments = rawDocuments2.Split(Environment.NewLine)

let repeat items =
    seq { while true do yield! items }

let directions =
    rawDocuments
        |> Array.head
        |> Seq.map (
            fun (ch: char) ->
                match ch with
                | 'L' -> Direction.Left
                | 'R' -> Direction.Right
                | _ -> raise <| ApplicationException($"unexpected direction: {ch}"))
        |> Seq.toArray

let graph =
    rawDocuments
        |> Array.skip 2
        |> Array.map (
            fun line ->
                match line with
                | Regexer nodeRegex [id; left; right] -> (id, { Id = id; LeftId = left; RightId = right })
                | _ -> raise <| ApplicationException($"invalid node: {line}"))
        |> Map.ofArray

let startingNodes =
    graph
        |> Map.filter (fun k v -> k.EndsWith("A"))
        |> Map.values
        |> Seq.toList

let findCycleLengths (node: Node) =
    let findNode (id: string) =
        match graph |> Map.tryFind id with
        | Some newNode -> newNode
        | None -> raise <| ApplicationException($"could not find node: {id}")
    let nextNode (node: Node) (directionIndex: int) =
        match directions[directionIndex] with
        | Direction.Left -> findNode node.LeftId
        | Direction.Right -> findNode node.RightId

    let rec countStepsUnit (node: Node) (predicate: Node -> bool) (steps: int64) (directionIndex: int) (show: bool) =
        if show then
            printfn "%s %d %d" node.Id steps directionIndex

        if predicate node then (steps, directionIndex, node)
        else countStepsUnit (nextNode node directionIndex) predicate (steps + 1L) ((directionIndex + 1) % directions.Length) show

    let (startToEndSteps, nextDirectionIndex, endNode1) = countStepsUnit node (fun x -> x.Id.EndsWith("Z")) 0L 0 false

    let nextNode = nextNode endNode1 nextDirectionIndex

    // printfn ">>> %s (%s %s) %d %d" endNode1.Id endNode1.LeftId endNode1.RightId startToEndSteps nextDirectionIndex
    // printfn ">>> nextNode: %s (%s %s)" (findNode nextNode.Id).Id (findNode nextNode.Id).LeftId (findNode nextNode.Id).RightId

    let (endToEndSteps, _, endNode2) = countStepsUnit (findNode nextNode.Id) (fun x -> x.Id.EndsWith("Z")) 0L ((nextDirectionIndex + 1) % directions.Length) false

    // printfn ">>> %s %d" endNode2.Id endToEndSteps

    (startToEndSteps, endNode1, endToEndSteps + 1L, endNode2)

printfn "Starting Nodes:"
printfn "%s" <| String.Join(", ", startingNodes |> List.map (fun x -> x.Id))
printfn ""

printfn "Cycle Lengths (by starting node):"

startingNodes
    |> List.map (
        fun node ->
            let (startToEndSteps, endNode1, endToEndSteps, endNode2) = findCycleLengths node
            (node, startToEndSteps, endNode1, endToEndSteps, endNode2))
    |> List.iter (
        fun (startNode, startToEndSteps, endNode1, endToEndSteps, endNode2) ->
            printfn "%s to %s %d steps, %s to %s %d steps" startNode.Id endNode1.Id startToEndSteps endNode1.Id endNode2.Id endToEndSteps)

printfn ""

// The cycle lengths for each of the starting nodes (as seen in the prior output) each have a start-to-end cycle length that is equal
// to the cycle length to once again get back to the end node. So, each starting node gets to its end node, over and over again, in a cycle.
//
// So, we can keep calculating multiples of each stating node's cycle length until we find a number that is a multiple of every starting node's
// cycle length.  That will be the same as the number of steps that sees every starting node land on its end node, all at the same time.
//
// This amounts to calculating the Least Common Multiple of all of the cycle lengths of the starting nodes.  Since LCM (of two numbers) is
// associative, it is valid to reduce/fold the LCM function over the list of cycle lengths.

// LCM algorithm from https://gist.github.com/krishnabhargav/da6686e295638d000aab
let rec gcd (a: int64) (b: int64) =
    match (a,b) with
    | (x,y) when x = y -> x
    | (x,y) when x > y -> gcd (x-y) y
    | (x,y) -> gcd x (y-x)

let lcm a b = a*b/(gcd a b)

let startingNodesCycleLengths =
    startingNodes
        |> List.map(
            fun node ->
                let (cycleLength, _, _, _) = findCycleLengths node
                cycleLength)

let lcmOfStartingNodesCycleLengths = startingNodesCycleLengths |> List.reduce lcm

printfn "%d" lcmOfStartingNodesCycleLengths
