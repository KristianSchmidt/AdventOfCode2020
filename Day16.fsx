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

let getColumn i =
    validNearbyTickets |> Array.map (fun a -> a.[i])

let constraints2 = constraintsTxt |> Array.map extractConstraints

let constraintsFulfilled constraints data =
    constraints
    |> Array.filter
        (fun (_,[|(d1,d2);(d3,d4)|]) ->
            data
            |> Array.forall (fun i -> d1 <= i && i <= d2 ||
                                      d3 <= i && i <= d4))

let solve2 constraints =
    let rec f cons found cols =
        if (Set.isEmpty cols) then
            found
        else
            let newCol,colNum =
                cols
                |> Set.toArray
                |> Array.map (fun i ->
                    i,(getColumn i |> constraintsFulfilled cons)
                    )
                |> Array.find (snd >> Array.length >> (=)1)
                |> (fun (i,[|s,_|]) -> s,i)

            let newFound = found |> Map.add newCol colNum
            let newCols = cols |> Set.remove colNum
            let newCons = cons |> Array.filter (fst >> (<>)newCol)
            f newCons newFound newCols
            
    f constraints Map.empty (Set.ofArray [|0..(constraints.Length-1)|])

let myTicket =
    let idx = Array.IndexOf(data, "your ticket:")
    data.[idx+1] |> split "," |> Array.map int
 
let ans2 =
    solve2 constraints2
    |> Map.toArray
    |> Array.filter (fst >> (fun s -> s.StartsWith("departure")))
    |> Array.map (snd >> (Array.get myTicket) >> int64)
    |> Array.reduce (*)

ans2