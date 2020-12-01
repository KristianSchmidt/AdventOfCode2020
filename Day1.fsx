#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 1 |> Array.map int

let (sum1,sum2) =
    seq {
        for i in data do
            for j in data do
                yield (i,j)
    } |> Seq.find (fun (i,j) -> i+j=2020)

let ans1 = sum1 * sum2

ans1

/// Part 2

let (sum1',sum2',sum3') =
    seq {
        for i in data do
            for j in data do
                for k in data do
                    yield (i,j,k)
    } |> Seq.find (fun (i,j,k) -> i+j+k=2020)

let ans2 = sum1'*sum2'*sum3'

ans2