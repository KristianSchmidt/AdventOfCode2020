#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let toBinary c1 c0 (s : string) =
    Convert.ToInt32(s.Replace(c1, '1').Replace(c0, '0'), 2)

let data =
    Helpers.Web.getInput 5
    |> Array.map (fun s -> toBinary 'B' 'F' (s.Substring(0,7)), toBinary 'R' 'L' (s.Substring(7)))

let seatIds =
    data
    |> Array.map (fun (row,col) -> row*8+col)
    |> Array.sort

let ans1 = Array.max seatIds

ans1

/// Part 2

let firstId = Array.head seatIds

let ans2 =
    seatIds
    |> Array.mapi (fun i x -> x-(firstId+i), x)
    |> Array.find (fst >> (<>)0)
    |> (fun (_,x) -> x-1)

ans2