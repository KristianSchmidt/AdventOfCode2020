#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let [|pub1;pub2|] =
    Helpers.Web.getInput 25
    |> Array.map (int >> bigint)

let findLoopSize toFind subject =
    let rec f value i =
        let newVal = (value * subject) % 20201227I
        if (newVal = toFind) then
            i
        else
            f newVal (i+1)

    f 1I 1

let loop1 = findLoopSize pub1 7I
let loop2 = findLoopSize pub2 7I

let mutable value = 1I
for i in 1..loop2 do
    value <- (value * pub1) % 20201227I

let ans1 = value

ans1
