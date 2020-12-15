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
            lastNum
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

let solveFast starting lastTurn =
    let arr1 = Array.init lastTurn (fun _ -> 0)
    let arr2 = Array.init lastTurn (fun _ -> 0)
    starting |> Array.iteri (fun i num -> arr2.[num] <- (i+1))

    let rec f lastNum turn =
        if (turn = lastTurn + 1) then
            lastNum
        else
            let newNum =
                if (arr1.[lastNum] = 0) then
                    0
                else
                    arr2.[lastNum] - arr1.[lastNum]

            arr1.[newNum] <- arr2.[newNum]
            arr2.[newNum] <- turn

            f newNum (turn+1)

    f (Array.last starting) (Array.length starting + 1)
    
#time "on"
let ans2 = solve data 30_000_000

let ans2Fast = solveFast data 30_000_000

ans2, ans2Fast