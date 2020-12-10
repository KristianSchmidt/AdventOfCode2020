#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 10 |> Array.map int

let maxJolt = Array.max data

let ans1 =
    Array.concat [| data; [|0; maxJolt+3 |]|]
    |> Array.sort
    |> Array.windowed 2
    |> Array.map (fun [|a1;a2|] -> a2-a1)
    |> Array.countBy id
    |> Array.map snd
    |> Array.reduce (*)

ans1

/// Part 2

let vertices =
    Array.concat [| data; [|0; maxJolt+3 |]|]
    |> Array.sort

let answers = Array.init vertices.Length (fun _ -> 0L)

let pathsFromIdx i =
    let paths =
        [|i+1;i+2;i+3|]
        |> Array.filter (fun j -> j < vertices.Length && vertices.[j] - vertices.[i] <= 3)
        |> Array.sumBy (fun j -> answers.[j])

    answers.[i] <- paths
    paths

// Initial condition
answers.[answers.Length-1] <- 1L

let ans2 =
    [| 0 .. (answers.Length-2) |]
    |> Array.rev
    |> Array.map pathsFromIdx
    |> Array.last

ans2