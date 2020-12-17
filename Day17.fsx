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

let maxX = data |> Array.map (fst >> fst) |> Array.max
let maxY = data |> Array.map (fst >> snd) |> Array.max

let adj (x,y,z) =
    [|
    (x-1,y+1,z+1); (x,y+1,z+1);(x+1,y+1,z+1)
    (x-1,y,  z+1); (x,y,  z+1);(x+1,y,  z+1)
    (x-1,y-1,z+1); (x,y-1,z+1);(x+1,y-1,z+1);

    (x-1,y+1,z);   (x,y+1,z);  (x+1,y+1,z)
    (x-1,y  ,z);               (x+1,y,  z)
    (x-1,y-1,z);   (x,y-1,z);  (x+1,y-1,z);

    (x-1,y+1,z-1); (x,y+1,z-1); (x+1,y+1,z-1)
    (x-1,y,  z-1); (x,y,  z-1); (x+1,y,  z-1)
    (x-1,y-1,z-1); (x,y-1,z-1); (x+1,y-1,z-1);
    |]
    |> Array.filter (fun (x,y,z) -> x >= 0 && x < maxX && y >= 0 && y < maxY)

let allZcoords z =
    seq {
        for x in 0 .. maxX do
            for y in 0 .. maxY do
                yield (x,y,z)
    }
    |> Array.ofSeq

let cycleCoord state (x,y,z) =
    let currState =
        state
        |> Map.tryFind (x,y,z)
        |> Option.defaultValue '.'

    //printfn $"Currstate: {currState}"
    
    let neighbors =
        adj (x,y,z)
        |> Array.map (fun c -> Map.tryFind c state |> Option.defaultValue '.')
    
    //printfn "Neigh: %A" neighbors
    
    let activeNeighbors = neighbors |> Seq.filter ((=)'#') |> Seq.length
    //printfn "active: %i" activeNeighbors
    match currState with
    | '#' when activeNeighbors = 2 || activeNeighbors = 3 ->
        '#'
    | '.' when activeNeighbors = 3 ->
        '#'
    | _ -> '.'

let solve (data : ((int*int)*char) array) cycles =
    let startingMap =
        data
        |> Array.map (fun ((x,y),c) -> (x,y,0),c)
        |> Map.ofArray

    let rec f state cyclesLeft =
        if (cyclesLeft = 0) then
            state
        else
            let zMin,zMax =
                state
                |> Map.toSeq
                |> Seq.map (fun ((_,_,z),_) -> z)
                |> Seq.distinct
                |> Seq.toArray
                |> (fun arr -> (Array.min arr,Array.max arr))

            let newCoords =
                let oldCoords = state |> Map.toArray |> Array.map fst
                Array.concat
                    [| allZcoords (zMin-1)
                       oldCoords
                       allZcoords (zMax+1)
                    |]
                |> Array.distinct

            let newState =
                newCoords
                |> Array.map (fun c -> c, cycleCoord state c)
                |> Map.ofArray

            f newState (cyclesLeft-1)

    f startingMap cycles

let ans1 =
    solve data 6
    |> Map.toArray
    |> Array.map snd
    |> Array.filter ((=)'#')
    |> Array.length


solve data 1
|> Map.filter (fun (x,y,z) c -> x = 1 && y = 0)

let startState = data |> Array.map (fun ((x,y),c) -> (x,y,0),c) |> Map.ofArray

cycleCoord startState (1,0,-1)

adj (1,0,-1)
ans1

/// Part 2

let ans2 = data

ans2