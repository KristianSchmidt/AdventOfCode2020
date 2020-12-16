#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 13

let limit = Array.head data |> int64

let ids =
    split "," data.[1]
    |> Array.filter ((<>)"x")
    |> Array.map int64

let closestTime id =
    let mul = int64 (float limit / float id) + 1L
    id * mul - limit

let ans1 =
    ids
    |> Array.map (fun i ->  i, closestTime i)
    |> Array.minBy snd
    |> fun (id,diff) -> id*diff

ans1

/// Part 2

let input =
    split "," data.[1]
    |> Array.mapi (fun i s -> i,s)
    |> Array.filter (snd >> ((<>)"x"))
    |> Array.map (fun (i,n) -> (bigint i), (bigint (int64 n)))

let test (adva, pa) (advb, pb) =
    let (_,s,t) = extGCD pa pb
    let z = adva-advb
    let m = z * s
    let n = -z * t
    let x = m*pa-adva
    let pc = bigint (lcm (int64 pa) (int64 pb))
    if (x < 0I) then
        let mults = abs (x / pc) + 1I
        (x + (mults * pc)) % pc, pc
    else
        x % pc, pc

let solve inputs =
    let head = Array.head inputs
    let tail = Array.tail inputs
    let rec f (adv, lcmul) offset inputs =
        match inputs with
        | [] -> int64 offset
        | x :: xs ->
            let (newOffset, newMul) = test (adv, lcmul) x
            let newAdv = newMul - newOffset
            f (newAdv, newMul) newOffset xs

    f head 0I (List.ofArray tail)

solve [| (0I, 17I); (2I, 13I); (3I, 19I)|]

let ans2 = solve input

ans2