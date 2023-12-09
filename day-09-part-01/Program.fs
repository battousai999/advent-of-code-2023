open System.IO
open System
open System.Linq

let rawReport = File.ReadAllLines("report.txt")

let report = 
    rawReport 
        |> Array.map (
            fun line -> 
                line.Split(' ')
                    |> Array.map Int32.Parse
                    |> Array.toList)
        |> Array.toList

let rec predictValue (history: int list) =
    let rec last list =
        match list with
        | [x] -> x
        | _::tail -> last tail
        | _ -> failwith "empty list"
    let (subHistory, _) = 
        history
            |> List.fold (
                fun ((results, prevValue): ResizeArray<int> * int option) (x: int) ->
                    match prevValue with
                    | Some value ->
                        results.Add(x - value)
                        (results, Some x)
                    | None ->
                        (results, Some x))
                (new ResizeArray<int>(), None)
    
    if subHistory.All(fun x -> x = 0) then
        (history |> last)
    else
        (history |> last) + (predictValue (subHistory |> Seq.toList))
    
let sumOfPredictedValues = 
    report 
        |> List.map predictValue
        |> List.sum

printfn "%d" sumOfPredictedValues


