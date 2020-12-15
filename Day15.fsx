#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 15 |> Array.head |> split "," |> Array.map int

let solve starting lastTurn =
    let initMap = starting |> Array.mapi (fun i x -> x,(0,i+1)) |> Map.ofArray

    let rec f state lastNum turn =
        //if (turn % 100_000 = 0) then
        //    printfn "Turn: %i" turn

        if (turn = lastTurn + 1) then
            printfn "Finished: %i" lastNum
            state
        else
            let newNum =
                match Map.tryFind lastNum state with
                | Some (0,i) -> 0
                | Some (i1,i2) -> i2-i1
            
            let newTurnsSeen =
                match Map.tryFind newNum state with
                | Some (i1,i2) -> (i2,turn)
                | None -> (0,turn)

            let newMap = state |> Map.add newNum newTurnsSeen

            f newMap newNum (turn+1)

    f initMap (Array.last starting) (Array.length starting + 1)

let ans1 = solve data 2020

ans1

/// Part 2

#time "on"
let ans2 = solve data 30000000

ans2