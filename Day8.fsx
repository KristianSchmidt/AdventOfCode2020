#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Instr =
    | Acc of int
    | Nop of int
    | Jmp of int

let parseInstruction (s : string) =
    let (name,arg) = s.Substring(0,3), int (s.Substring(3))
    match name with
    | "acc" -> Acc arg
    | "nop" -> Nop arg
    | "jmp" -> Jmp arg

let data = Helpers.Web.getInput 8 |> Array.map parseInstruction

let solve (data : Instr array) =
    let rec loop idx acc seen =
        if (Set.contains idx seen) then
            acc
        else
            let newSeen = Set.add idx seen
            let instr = data.[idx]
            match instr with
            | Nop _ -> loop (idx+1) acc newSeen
            | Acc arg -> loop (idx+1) (acc+arg) newSeen
            | Jmp arg -> loop (idx+arg) acc newSeen

    loop 0 0 Set.empty

let ans1 = solve data

ans1

/// Part 2

let solve2 (data : Instr array) =
    let rec loop idx acc seen =
        if (Set.contains idx seen) then
            None
        else if (idx = data.Length) then
            Some acc
        else
            let newSeen = Set.add idx seen
            let instr = data.[idx]
            match instr with
            | Nop _ -> loop (idx+1) acc newSeen
            | Acc arg -> loop (idx+1) (acc+arg) newSeen
            | Jmp arg -> loop (idx+arg) acc newSeen

    loop 0 0 Set.empty

let replaceAt (data : Instr array) idx =
    let newElement =
        match data.[idx] with
        | Nop arg -> Jmp arg
        | Jmp arg -> Nop arg
    data |> Array.mapi (fun i e -> if (i = idx) then newElement else e)

let replaceIdxes =
     data
     |> Array.mapi (fun i e -> match e with | Nop _ | Jmp _ -> Some i | _ -> None)
     |> Array.choose id

let newPrograms =
    replaceIdxes
    |> Array.map (replaceAt data)

let ans2 = newPrograms |> Array.choose solve2 |> Array.head

ans2