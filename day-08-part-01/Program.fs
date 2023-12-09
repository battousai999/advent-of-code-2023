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

let rawDocuments = File.ReadAllLines("documents.txt")

// let rawDocuments2 = @"LLR

// AAA = (BBB, BBB)
// BBB = (AAA, ZZZ)
// ZZZ = (ZZZ, ZZZ)"
//
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
        //|> repeat
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

let followDirections (startingNode: Node) (directions: Direction array) =
    let mutable steps: int64 = 0
    let mutable currentNode: Node = startingNode
    let mutable directionIndex: int = 0
    let findNextNode (id: string) =
        match graph |> Map.tryFind id with
        | Some newNode -> newNode
        | None -> raise <| ApplicationException($"could not find node: {id}")

    while currentNode.Id <> "ZZZ" do
        let direction = directions[directionIndex]
        let nextNode =
            match direction with
            | Direction.Left -> findNextNode currentNode.LeftId
            | Direction.Right -> findNextNode currentNode.RightId
        
        directionIndex <- if directionIndex = directions.Length - 1 then 0 else directionIndex + 1
        steps <- steps + 1L
        currentNode <- nextNode

    steps

let startingNode =
    match graph |> Map.tryFind "AAA" with
    | Some node -> node
    | None -> raise <| ApplicationException($"could not find starting node")

let results = followDirections startingNode directions
    
printfn "%d" results
