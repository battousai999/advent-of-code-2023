open System
open System.IO
open System.Text.RegularExpressions

type RatingType =
| Cool
| Musical
| Aero
| Shiny

type Destination =
| WorkflowDestination of string
| Accepted
| Rejected

type ConditionedRoute = {
    RatingType: RatingType
    Operator: int -> int -> bool
    RatingValue: int
    Destination: Destination
}

type Route =
| ConditionedRoute of ConditionedRoute
| DestinationRoute of Destination

type Workflow = {
    Name: string
    Routes: Route list
}

type Part = {
    CoolRating: int
    MusicalRating: int
    AeroRating: int
    ShinyRating: int
}

let (|Regexer|_|) (regex : Regex) input =
    let m = regex.Match(input)

    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let workflowRegex = Regex(@"^(\w+)\{([^\}]+)\}$")
let workflowElementRegex = Regex(@"^(\w)([><])(\d+):(\w+)$")
let ratingRegex = Regex(@"^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$")

let rawSpecifications = File.ReadAllText("workflows-and-ratings.txt")

let (rawWorkflows, rawParts) =
    let splitElements = rawSpecifications.Split(Environment.NewLine + Environment.NewLine)
    (splitElements[0], splitElements[1])

let workflows =
    rawWorkflows.Split(Environment.NewLine)
        |> Array.map (
            fun line ->
                match line with
                | Regexer workflowRegex [name; specs] ->
                    let rawWorkflowRoutes = specs.Split(',')
                    let workflowRoutes =
                        rawWorkflowRoutes
                            |> Array.map (
                                fun route ->
                                    match route with
                                    | Regexer workflowElementRegex [rawRating; rawOperator; rawRatingValue; rawDestination] ->
                                        let rating =
                                            match rawRating[0] with
                                            | 'x' -> RatingType.Cool
                                            | 'm' -> RatingType.Musical
                                            | 'a' -> RatingType.Aero
                                            | 's' -> RatingType.Shiny
                                            | _ -> raise <| ApplicationException($"unexpected rating type: {rawRating[0]}")
                                        let operator =
                                            match rawOperator[0] with
                                            | '<' -> (<)
                                            | '>' -> (>)
                                            | _ -> raise <| ApplicationException($"unexpected operator: {rawOperator[0]}")
                                        let ratingValue = rawRatingValue |> Int32.Parse
                                        let destination =
                                            match rawDestination with
                                            | "A" -> Destination.Accepted
                                            | "R" -> Destination.Rejected
                                            | dest when dest <> "" -> Destination.WorkflowDestination dest
                                            | _ -> raise <| ApplicationException($"unexpected destination: {rawDestination}")
                                        ConditionedRoute { RatingType = rating; Operator = operator; RatingValue = ratingValue; Destination = destination }
                                    | destination ->
                                        match destination with
                                        | "A" -> DestinationRoute Destination.Accepted
                                        | "R" -> DestinationRoute Destination.Rejected
                                        | dest when dest <> "" -> DestinationRoute (Destination.WorkflowDestination dest)
                                        | _ -> raise <| ApplicationException($"unexpected destination: {destination}"))
                            |> Array.toList
                    { Name = name; Routes = workflowRoutes }
                | _ -> raise <| ApplicationException($"invalid workflow: {line}"))
        |> Array.toList

let workflowStart = workflows |> List.find (fun workflow -> workflow.Name = "in")

let parts =
    rawParts.Split(Environment.NewLine)
        |> Array.map (
            fun line ->
                match line with
                | Regexer ratingRegex [rawCoolRating; rawMusicalRating; rawAeroRating; rawShinyRating] ->
                    let coolRating = rawCoolRating |> Int32.Parse
                    let musicalRating = rawMusicalRating |> Int32.Parse
                    let aeroRating = rawAeroRating |> Int32.Parse
                    let shinyRating = rawShinyRating |> Int32.Parse
                    { CoolRating = coolRating; MusicalRating = musicalRating; AeroRating = aeroRating; ShinyRating = shinyRating }
                | _ -> raise <| ApplicationException($"invalid part: {line}"))
        |> Array.toList

let isPartAccepted (part: Part) =
    let rec runWorkflows (currentWorkflow: Workflow) =
        let matchingRoute =
            currentWorkflow.Routes
                |> List.find (
                    fun route ->
                        match route with
                        | ConditionedRoute { RatingType = ratingType; Operator = operator; RatingValue = ratingValue; Destination = _ } ->
                            let rating =
                                match ratingType with
                                | RatingType.Cool -> part.CoolRating
                                | RatingType.Musical -> part.MusicalRating
                                | RatingType.Aero -> part.AeroRating
                                | RatingType.Shiny -> part.ShinyRating
                            operator rating ratingValue
                        | DestinationRoute _ -> true)
        match matchingRoute with
        | ConditionedRoute { Destination = Destination.Accepted }
        | DestinationRoute Destination.Accepted ->
            true
        | ConditionedRoute { Destination = Destination.Rejected }
        | DestinationRoute Destination.Rejected ->
            false
        | ConditionedRoute { Destination = Destination.WorkflowDestination dest }
        | DestinationRoute (Destination.WorkflowDestination dest) ->
            let newWorkflow = workflows |> List.find (fun workflow -> workflow.Name = dest)
            runWorkflows newWorkflow

    runWorkflows workflowStart

let sumOfAcceptedParts =
    parts
        |> List.filter isPartAccepted
        |> List.sumBy (fun part -> part.CoolRating + part.MusicalRating + part.AeroRating + part.ShinyRating)

printfn "%d" sumOfAcceptedParts
