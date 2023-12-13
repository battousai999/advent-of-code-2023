open System
open System.IO
open System.Text.RegularExpressions
open System.Threading.Tasks
open System.Threading

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
    // Consider adding as-it-goes checking against the contiguous pattern in order to
    // optimize away arrangements that will obviously not be valid
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
let mutable results = Array.init (conditionRecords |> List.length) (fun _ -> 0L)

let threads =
    conditionRecords
        |> List.mapi (
            fun i x ->
                let work _ =
                    let conditionRecord = conditionRecordsArray[i]
                    let arrangementCount = conditionRecord |> calculateValidArrangements |> Seq.length
                    results[i] <- (arrangementCount |> int64)
                    printfn "%A --> count %d" conditionRecord.OriginalConditionsStr arrangementCount
                let thread = Thread(ThreadStart(work))

                thread.Start()
                thread)

threads |> List.iter (fun thread -> thread.Join())

let sumValue = results |> Array.sum

printfn ""
printfn "%d" sumValue

