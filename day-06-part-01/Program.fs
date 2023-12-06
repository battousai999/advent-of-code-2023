type Race = {
    Time: int
    RecordDistance: int
}

let races = [
    { Time = 46; RecordDistance = 347 }
    { Time = 82; RecordDistance = 1522 }
    { Time = 84; RecordDistance = 1406 }
    { Time = 79; RecordDistance = 1471 }
]

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
    let intMinValue = (if ceil(minValue) = minValue then ceil(minValue) + 1.0 else ceil(minValue)) |> int
    let intMaxValue = (if floor(maxValue) = maxValue then floor(maxValue) - 1.0 else floor(maxValue)) |> int

    (intMaxValue - intMinValue + 1)

let productOfWinCounts = races |> List.map numWinningButtonHoldTimes |> List.fold (fun x y -> x * y) 1

printfn "%A" productOfWinCounts
