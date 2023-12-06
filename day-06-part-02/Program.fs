open System

type Race = {
    Time: int64
    RecordDistance: int64
}

let races = [
    { Time = 46L; RecordDistance = 347L }
    { Time = 82L; RecordDistance = 1522L }
    { Time = 84L; RecordDistance = 1406L }
    { Time = 79L; RecordDistance = 1471L }
]

let actualRace = 
    let (time, recordDistance) = 
        races
            |> List.fold (
                fun (time, recordDistance) (race: Race) ->
                    let newTime = time + (race.Time |> string)
                    let newRecordDistance = recordDistance + (race.RecordDistance |> string)

                    (newTime, newRecordDistance))
                ("", "")
    { Time = time |> Int64.Parse; RecordDistance = recordDistance |> Int64.Parse }

// Note: for the equation `-RecordDistance + (Time * x) - x^2 = 0`, you get two solutions (between which
// the integer values represent the values which will result in wins):
//
// x_1 = ceil(0.5 * (rt - sqrt((rt * rt) - (4 * td))))
// x_2 = floor(0.5 * (rt + sqrt((rt * rt) - (4 * td))))
//
// with rt = Race.Time and td = Race.RecordDistance

let numWinningButtonHoldTimes (race: Race) =
    let rt = race.Time |> double
    let td = race.RecordDistance |> double
    let minValue = 0.5 * (rt - sqrt ((rt * rt) - (4.0 * td))) 
    let maxValue = 0.5 * (rt + sqrt ((rt * rt) - (4.0 * td))) 
    let intMinValue = (if ceil(minValue) = minValue then ceil(minValue) + 1.0 else ceil(minValue)) |> int64
    let intMaxValue = (if floor(maxValue) = maxValue then floor(maxValue) - 1.0 else floor(maxValue)) |> int64

    (intMaxValue - intMinValue + 1L)

printfn "%A" (numWinningButtonHoldTimes actualRace)
