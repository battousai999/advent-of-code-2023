open System
open System.IO
open System.Text.RegularExpressions
open System.Threading.Tasks

type Condition =
| Operational
| Damaged
| Unknown

type ConditionRecord = {
    Conditions: Condition array
    ContiguousPattern: int list
    OriginalConditionsStr: string
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let rawConditionRecords = File.ReadAllLines("../day-12-part-01/condition-records.txt")

// let rawConditionRecordsStr = @"???.### 1,1,3
// .??..??...?##. 1,1,3
// ?#?#?#?#?#?#?#? 1,3,1,6
// ????.#...#... 4,1,1
// ????.######..#####. 1,6,5
// ?###???????? 3,2,1"

// let rawConditionRecords = rawConditionRecordsStr.Split(Environment.NewLine)

let conditionRecordRegex = Regex(@"^([\.#\?]+)\s([\d,]+)$")

let conditionRecords =
    rawConditionRecords
        |> Array.map (
            fun line ->
                match line with
                | Regexer conditionRecordRegex [unfoldedConditions; unfoldedContiguousPattern] ->
                    let rawConditions = String.Join("?", (Seq.init 5 (fun _ -> unfoldedConditions)))
                    let rawContiguousPattern = String.Join(",", (Seq.init 5 (fun _ -> unfoldedContiguousPattern)))
                    let conditions =
                        rawConditions
                            |> Seq.map (
                                fun ch ->
                                    match ch with
                                    | '.' -> Condition.Operational
                                    | '#' -> Condition.Damaged
                                    | '?' -> Condition.Unknown
                                    | _ -> raise <| ApplicationException($"unexpected condition character ({ch}) in line: {line}"))
                            |> Seq.toArray
                    let contiguousPattern = rawContiguousPattern.Split(',') |> Array.map Int32.Parse |> Array.toList

                    { Conditions = conditions; ContiguousPattern = contiguousPattern; OriginalConditionsStr = unfoldedConditions }
                | _ -> raise <| ApplicationException($"invalid condition record: {line}"))
        |> Array.toList

let calculateValidArrangements (conditionRecord: ConditionRecord) =
    let possibleArrangements =
        let rec calculateArrangements (conditions: Condition list): Condition list seq =
            match conditions with
            | condition :: remainingConditions when condition = Condition.Unknown ->
                let possibleRemainingArrangements = calculateArrangements remainingConditions
                possibleRemainingArrangements
                    |> Seq.collect (
                        fun list ->
                            [(Condition.Operational :: list); (Condition.Damaged :: list)])
            | condition :: remainingConditions ->
                let possibleRemainingArrangements = calculateArrangements remainingConditions
                possibleRemainingArrangements |> Seq.map (fun list -> condition :: list)
            | [] -> [[]]

        calculateArrangements (conditionRecord.Conditions |> Array.toList)

    let isValidArrangement (conditions: Condition list) (contiguousPattern: int list) =
        let (initialCalculatedContiguousPattern, finalDamageCount) =
            conditions
                |> List.fold (
                    fun ((currentPattern, damagedCount): int list * int) (condition: Condition) ->
                        match (condition, damagedCount) with
                        | (Condition.Operational, 0) -> (currentPattern, 0)
                        | (Condition.Operational, damagedCount) when damagedCount > 0 -> ((List.append currentPattern [damagedCount]), 0)
                        | (Condition.Damaged, damagedCount) -> (currentPattern, damagedCount + 1)
                        | (Condition.Unknown, _) -> raise <| ApplicationException(sprintf "unexpected unknown condition: %A" conditions)
                        | _ -> raise <| ApplicationException(sprintf "unexpected case for condition (%A): %A" condition conditions))
                    ([], 0)

        let calculatedContiguousPattern =
            if finalDamageCount > 0 then
                List.append initialCalculatedContiguousPattern [finalDamageCount]
            else
                initialCalculatedContiguousPattern

        // printfn "%A" conditions
        // printfn "calculated = %A, pattern = %A" calculatedContiguousPattern contiguousPattern
        // printfn ""
        calculatedContiguousPattern = contiguousPattern

    possibleArrangements |> Seq.filter (fun x -> isValidArrangement x conditionRecord.ContiguousPattern)

printfn "calculating valid arrangements..."

let conditionRecordsArray = conditionRecords |> List.toArray
let mutable sumValue = 0L;
let sumValueLock = Object()

Parallel.For(
    fromInclusive = 0,
    toExclusive = (conditionRecordsArray |> Array.length),
    localInit = (fun () -> 0L),
    body = (fun (value: int) (state: ParallelLoopState) (local: int64) ->
        let conditionRecord = conditionRecordsArray[value]
        let arrangementCount = conditionRecord |> calculateValidArrangements |> Seq.length
        printfn "%A --> count %d" conditionRecord.OriginalConditionsStr arrangementCount
        local + (arrangementCount |> int64)),
    localFinally = (fun local -> lock sumValueLock (fun () -> sumValue <- sumValue + local))) |> ignore

printfn ""
printfn "%d" sumValue

// let sumOfValidArragementCounts =
//     conditionRecords
//         //|> List.map (calculateValidArrangements >> List.length)
//         |> List.mapi (
//             fun i x ->
//             let arrangementCount = x |> calculateValidArrangements |> Seq.length
//             printfn "record #%d: count %d" (i + 1) arrangementCount
//             arrangementCount)
//         |> List.sum

// printfn "%d" sumOfValidArragementCounts
