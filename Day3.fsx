#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 3
    |> Array.map (fun s -> s.ToCharArray())

let (xLength, yLength) = (data.[0].Length, data.Length)

let solve (xInc, yInc) =
    let rec loop (x,y) count =
        if (y >= yLength) then
            count
        else
            let c = data.[y].[x]
            let nextX = (x + xInc) % xLength
            match c with
            | '.' -> loop (nextX, y+yInc) count
            | '#' -> loop (nextX, y+yInc) (count+1)

    loop (xInc,yInc) 0

let ans1 = solve (3,1)

ans1

/// Part 2

let ans2 =
    [| (1,1); (3,1); (5,1); (7,1); (1,2) |]
    |> Array.map (solve >> int64)
    |> Array.reduce (*)

ans2