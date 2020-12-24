#load "Helpers.fsx"

open System
open System.Collections.Generic
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = "589174263".ToCharArray() |> Array.map (string >> int)
//let data = "389125467".ToCharArray() |> Array.map (string >> int)

let x = ResizeArray<int>(data)
let mutable currLabel = x.[0]
for i in 0..99 do
    let idx = x.FindIndex(fun i -> i = currLabel)
    let nextLabel = x.[(idx+4) % data.Length]
    printfn "Idx: %i" idx
    printfn "NextLabel: %i" nextLabel

    let nextThree =
        let idxs = [|idx+1;idx+2;idx+3|]
                   |> Array.map (fun i -> i % data.Length)
        let vals = idxs |> Array.map (fun i -> x.[i])
        idxs |> Array.sortDescending |> Array.iter (fun i -> x.RemoveAt(i))
        vals
        //x.GetRange(idx+1,3)
    //x.RemoveRange(idx+1,3)
    printf "Pick up: "
    nextThree |> Seq.iter (printf "%i ")
    printfn ""

    let rec findInsertIndex labelToFind =
        //printfn "Trying to find %i" labelToFind
        let idx = x.FindIndex(fun i -> i = labelToFind)
        if (idx = -1) then
            let nextLabel = labelToFind - 1
            if (nextLabel <= 0) then
                let max = Seq.max x
                findInsertIndex max
            else
                findInsertIndex nextLabel
        else
            idx+1

    let insertIdx = findInsertIndex (currLabel - 1)
    x.InsertRange(insertIdx,nextThree)

    currLabel <- nextLabel

    x |> Seq.iter (printf "%i ")
    printfn ""
    printfn ""

let ans1 =
    let idx1 = x.FindIndex(fun i -> i = 1)
    [|1..data.Length-1|]
    |> Array.map (fun i -> (idx1 + i) % data.Length)
    |> Array.map (fun i -> x.[i])
    |> (fun arr -> String.Join("",arr))

ans1

/// Part 2

let y =
    let maxOrigElement = Array.max data
    let orig = Array.pairwise data
    let extra = [| (maxOrigElement + 1) .. 1_000_000 |] |> Array.pairwise
    let binders = [| (data.[data.Length-1], maxOrigElement + 1); (1_000_000,data.[0]) |]
    Array.concat [|orig;extra;binders|]
    |> Map.ofArray

let findNextThree (map : Map<int,int>) elem =
    let fstNeighbor = Map.find elem map
    let sndNeighbor = Map.find fstNeighbor map
    let trdNeighbor = Map.find sndNeighbor map
    [fstNeighbor;sndNeighbor;trdNeighbor]

let solve (map : Map<int,int>) iters firstElem =
    let maxElem = map |> Map.toSeq |> Seq.map fst |> Seq.max

    let rec getDestCup guess neighbors =
        if (List.contains guess neighbors) then
            getDestCup (guess-1) neighbors
        else if (guess = 0) then
            getDestCup maxElem neighbors
        else
            guess

    let rec doIter (map : Map<int,int>) iterLeft currLabel =
        if (iterLeft = 0) then
            map
        else
            if (iterLeft % 100_000 = 0) then printfn "Iter: %i" iterLeft

            let neighbors = findNextThree map currLabel
            let [fstNeighbor;sndNeighbor;trdNeighbor] = neighbors
            let nextLabel = Map.find trdNeighbor map

            let newMap1 = Map.add currLabel nextLabel map
            let insertionLabel = getDestCup (currLabel-1) neighbors
            let insertionLabelNeighbor = Map.find insertionLabel map
            let newMap2 =
                Map.add insertionLabel fstNeighbor newMap1
                |> Map.add trdNeighbor insertionLabelNeighbor
        
            doIter newMap2 (iterLeft-1) nextLabel

    doIter map iters firstElem

let finalMap = solve y 10_000_000 data.[0]

let ans2 =
    let n1 = Map.find 1 finalMap
    let n2 = Map.find n1 finalMap
    (int64 n1) * (int64 n2)

ans2