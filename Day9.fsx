#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 9 |> Array.map int64

let isNotInLast25 i target =
    if (i < 25) then
        false
    else
        seq {
            for j in (i-25) .. i-2 do
                for k in (j+1) .. (i-1) do
                    yield data.[j] + data.[k]
        }
        |> Seq.tryFind ((=)target)
        |> Option.isNone

let ans1 =
    data
    |> Seq.mapi (fun i t -> t, isNotInLast25 i t)
    |> Seq.find snd
    |> fst

ans1

/// Part 2

let maxIdx = (data |> Seq.findIndex ((=)ans1)) - 1

let tryFindContiq startIdx =
    let rec f i nums acc =
        let newAcc = acc + data.[i]
        match newAcc with
        | _ when newAcc = ans1 -> Some (startIdx, i, nums)
        | _ when newAcc > ans1 -> None
        | _ -> f (i+1) (data.[i] :: nums) newAcc

    f startIdx [] 0L

let ans2 =
    [| 0 .. maxIdx |]
    |> Seq.choose tryFindContiq
    |> Seq.exactlyOne
    |> (fun (_,_,xs) -> List.max xs + List.min xs)
    

ans2