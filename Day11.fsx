#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 11

let init =
    data
    |> Array.mapi (fun y s -> s.ToCharArray() |> Array.mapi (fun x c -> ((x,y),c)))
    |> Array.collect id
    |> Map.ofArray

let adj map (x,y) =
    [| (x-1,y-1); (x, y-1); (x+1,y-1);
       (x-1,y); (x+1,y)
       (x-1,y+1); (x, y+1); (x+1, y+1)
    |]
    |> Array.choose (fun coord -> Map.tryFind coord map)

let transform adj limit map coord c =
    match c with
    | 'L' ->
        let hasNeighbors = adj map coord |> Array.filter ((=)'#') |> Array.length |> (<>)0
        if (hasNeighbors) then 'L' else '#'
    | '#' ->
        let shouldChange = adj map coord |> Array.filter ((=)'#') |> Array.length |> (fun x -> x >= limit)
        if (shouldChange) then 'L' else '#'
    | _ -> c

let solve transform initMap =
    let rec f map =
        let newMap = map |> Map.map (fun coord c -> transform map coord c)
        if (newMap = map) then newMap else f newMap

    f initMap
    |> Map.toArray
    |> Array.map snd
    |> Array.filter ((=)'#')
    |> Array.length

let ans1 = solve (transform adj 4) init

ans1

/// Part 2

let adj2 map (x,y) =
    let vectors = [| (-1,-1); (0, -1); (1,-1);
                     (-1,0); (1,0)
                     (-1,1); (0, 1); (1, 1) |]

    let findFirst (startX,startY) (vecX,vecY) =
        let rec f (x,y) =
            match Map.tryFind (x,y) map with
            | Some '.' -> f ((x+vecX),(y+vecY))
            | opt -> opt

        f (startX + vecX, startY + vecY)

    vectors |> Array.choose (findFirst (x,y))

let ans2 = solve (transform adj2 5) init

ans2