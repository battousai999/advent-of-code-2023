open System.IO
open System
open System.Linq

let rawReport = File.ReadAllLines("../day-09-part-01/report.txt")

let report = 
    rawReport 
        |> Array.map (
            fun line -> 
                line.Split(' ')
                    |> Array.map Int32.Parse
                    |> Array.toList)
        |> Array.toList

let rec predictInitialValue (history: int list) =
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
        (history |> List.head)
    else
        (history |> List.head) - (predictInitialValue (subHistory |> Seq.toList))
    
let sumOfPredictedValues = 
    report 
        |> List.map predictInitialValue
        |> List.sum

printfn "%d" sumOfPredictedValues


