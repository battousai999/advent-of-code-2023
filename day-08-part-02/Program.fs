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
//
//let rawDocuments = rawDocuments2.Split(Environment.NewLine)

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

let followDirections (startingNodes: Node list) (directions: Direction array) =
    let mutable steps: int64 = 0
    let mutable currentNodes: Node list = startingNodes
    let mutable directionIndex: int = 0
    let findNextNode (id: string) =
        match graph |> Map.tryFind id with
        | Some newNode -> newNode
        | None -> raise <| ApplicationException($"could not find node: {id}")
    let allOnEndingNodes (nodes: Node list) = nodes |> List.forall (fun x -> x.Id.EndsWith("Z"))

    while not (allOnEndingNodes currentNodes) do
        let direction = directions[directionIndex]
        let nextNode (node: Node) =
            match direction with
            | Direction.Left -> findNextNode node.LeftId
            | Direction.Right -> findNextNode node.RightId

        //printfn "%A %A" (direction) (currentNodes)
        if steps % 100000000L = 0 then printfn "%d" steps
        
        directionIndex <- if directionIndex = directions.Length - 1 then 0 else directionIndex + 1
        steps <- steps + 1L
        currentNodes <- currentNodes |> List.map nextNode

    steps

let startingNodes =
    graph 
        |> Map.filter (fun k v -> k.EndsWith("A"))
        |> Map.values
        |> Seq.toList

let results = followDirections startingNodes directions
    
printfn "%d" results
