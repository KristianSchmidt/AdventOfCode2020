#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 17
    |> Array.mapi
        (fun y arr ->
            arr.ToCharArray()
            |> Array.mapi (fun x c -> ((x,y),c)))
    |> Array.collect id

let dim3combinations = product [-1;0;1] 3 |> List.filter ((<>)[0;0;0]) |> Array.ofList

let adj (x,y,z) =
    dim3combinations
    |> Array.map (fun [x';y';z'] -> (x+x',y+y',z+z'))

let cycleCoord adj state coord =
    let currState =
        state
        |> Map.tryFind coord
        |> Option.defaultValue '.'
            
    let neighbors =
        adj coord
        |> Array.map (fun c -> Map.tryFind c state |> Option.defaultValue '.')
        
    let activeNeighbors = neighbors |> Seq.filter ((=)'#') |> Seq.length
    
    match currState with
    | '#' when activeNeighbors = 2 || activeNeighbors = 3 -> '#'
    | '.' when activeNeighbors = 3 -> '#'
    | _ -> '.'

let solve startingMap adj cycles =
    let rec f state cyclesLeft =
        if (cyclesLeft = 0) then
            state |> Map.filter (fun k v -> v = '#') |> Map.count
        else
            let newCoords =
                let keyValues = Map.toArray state
                let oldActive = keyValues |> Array.filter (snd >> (=)'#') |> Array.map fst
                let newCandidates = oldActive |> Array.collect adj // neighbors of all actives
                let oldCoords = keyValues |> Array.map fst
                Array.concat [| oldCoords; newCandidates |]
                |> Array.distinct

            let newState =
                newCoords
                |> Array.map (fun c -> c, cycleCoord adj state c)
                |> Map.ofArray

            f newState (cyclesLeft-1)

    f startingMap cycles

let startingMap =
    data
    |> Array.map (fun ((x,y),c) -> (x,y,0),c)
    |> Map.ofArray

let ans1 = solve startingMap adj 6

ans1

/// Part 2

let dim4combinations = product [-1;0;1] 4 |> List.filter ((<>)[0;0;0;0]) |> Array.ofList

let adj2 (x,y,z,w) =
    dim4combinations
    |> Array.map (fun [x';y';z';w'] -> (x+x',y+y',z+z',w+w'))

let startingMap2 =
    data
    |> Array.map (fun ((x,y),c) -> (x,y,0,0),c)
    |> Map.ofArray

let ans2 = solve startingMap2 adj2 6

ans2