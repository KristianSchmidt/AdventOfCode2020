#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 6

let groups =
    let rec loop p (grps : string list list) currGrp =
        match p with
        | x :: [] ->
            (x :: currGrp) :: grps
        | x :: xs ->
            match x with 
            | "" -> loop xs (currGrp :: grps) []
            | s -> loop xs grps (s :: currGrp)

    loop (List.ofArray data) [] []
    |> Array.ofList
    |> Array.map Array.ofList

let ans1 =
    groups
    |> Array.map (Array.collect (fun s -> s.ToCharArray()))
    |> Array.sumBy (Array.distinct >> Array.length)
    
ans1

/// Part 2

let processGroup (grp : string array) =
    let questions = grp |> Array.collect (fun s -> s.ToCharArray())
                        |> Array.distinct
                        |> Array.map string

    let allHasAnswered q = grp |> Array.forall (fun s -> s.Contains(q))

    questions |> Array.filter allHasAnswered |> Array.length

let ans2 = groups |> Array.sumBy processGroup

ans2