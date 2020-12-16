#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


let data = Helpers.Web.getInput 16

let constraintsTxt =
    let constraintsIdx = Array.IndexOf(data, "")
    data.[0..constraintsIdx-1]

let extractConstraints =
    function
    | Regex "(.+): (\d+)\-(\d+) or (\d+)\-(\d+)" [name;d1;d2;d3;d4] -> (name,[|(int d1,int d2);(int d3,int d4)|])

let constraints = constraintsTxt |> Array.collect (extractConstraints >> snd)

let fieldIsInvalid i =
    constraints
    |> Array.map (fun (d1,d2) -> d1 <= i && i <= d2)
    |> Array.forall ((=)false)

let nearbyTickets =
    let nearbyIdx = Array.IndexOf(data, "nearby tickets:")
    data.[nearbyIdx+1..]
    |> Array.map (split "," >> Array.map int)

let ticketIsInvalid (ticketData : int array) =
    let rec f rest errRate =
        match rest with
        | [] ->
            errRate > 0, errRate
        | x :: xs ->
            if (fieldIsInvalid x) then
                f xs (errRate + x)
            else
                f xs errRate

    f (List.ofArray ticketData) 0

let ans1 = nearbyTickets |> Array.sumBy (ticketIsInvalid >> snd)

ans1

/// Part 2

let validNearbyTickets =
    nearbyTickets
    |> Array.filter (ticketIsInvalid >> fst >> not)

let constraints2 = constraintsTxt |> Array.map extractConstraints

let ans2 = data

ans2