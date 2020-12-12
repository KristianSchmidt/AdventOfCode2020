#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Action = | N of int | S of int | E of int | W of int | R of int | F of int

let cos' a = (int (cos ((float a) * Math.PI / 180.)))
let sin' a = (int (sin ((float a) * Math.PI / 180.)))

// Clockwise rotation
let rotate (v: V) a =
    V(v.x * cos' a + v.y * sin' a, -v.x * sin' a + v.y * cos' a)

let parseAction =
    function
    | Regex "N(\d+)" [d] -> N (int d)
    | Regex "S(\d+)" [d] -> S (int d)
    | Regex "E(\d+)" [d] -> E (int d)
    | Regex "W(\d+)" [d] -> W (int d)
    | Regex "L(\d+)" [d] -> R (360 - int d)
    | Regex "R(\d+)" [d] -> R (int d)
    | Regex "F(\d+)" [d] -> F (int d)

let (|Dir|_|) =
    function
    | N t -> Some (V(0,t)) | S t -> Some (V(0,-t))
    | E t -> Some (V(t,0)) | W t -> Some (V(-t,0))
    | _ -> None

let data = Helpers.Web.getInput 12 |> Array.map parseAction |> List.ofArray

let solve instrs =
    let rec f instr (dir: V) (ship: V) =
        match instr with
        | [] -> ship.l1norm
        | x :: xs ->
            match x with
            | Dir v -> f xs dir (ship + v)
            | F t   -> f xs dir (ship + dir * t)
            | R a   -> f xs (rotate dir a) ship

    f instrs (V(1,0)) (V(0,0))

let ans1 = solve data

/// Part 2

let solve2 instrs =
    let rec f instr (wp: V) (ship: V) =
        match instr with
        | [] -> ship.l1norm
        | x :: xs ->
            match x with
            | Dir v -> f xs (wp + v) ship
            | F t   -> f xs wp (ship + wp * t)
            | R a   -> f xs (rotate wp a) ship

    f instrs (V(10,1)) (V(0,0))

let ans2 = solve2 data

ans2

