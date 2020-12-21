#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 21
   
let allSuspects =
    data
    |> Array.collect (
        function
        | Regex "^(.+) \(contains (.+)\)$" [ingredients;allergens]
            -> let aller = allergens |> split ", "
               let ins = Set.ofArray (ingredients |> split " ")
               aller |> Array.map (fun a -> a,ins)
        )

let processGrp (s, arr) =
    let suspects = arr |> Array.map snd |> Set.intersectMany
    s,suspects

let suspects =
    allSuspects
    |> Array.groupBy fst
    |> Array.map processGrp
    |> Array.map snd
    |> Set.unionMany

let ans1 =
    data
    |> Array.collect (
        function
        | Regex "^(.+) \(contains (.+)\)$" [ingredients;allergens]
            -> ingredients |> split " "
        )
    |> Array.sumBy (fun i -> if (Set.contains i suspects) then 0 else 1)

ans1

/// Part 2

let solve (grps : (string * Set<string>) array) =
    let rec f state acc =
        if (Array.length state = 0) then
            acc
        else
            let allergen,ingr =
                state
                |> Array.find (snd >> Set.count >> ((=)1))
                |> (fun (al,i) -> al, Seq.exactlyOne i)
    
            let newAcc = (allergen,ingr) :: acc
            let newState =
                state
                |> Array.map (fun (al,i) -> al,Set.remove ingr i)
                |> Array.filter (snd >> Set.count >> (fun i -> i > 0))

            f newState newAcc

    f grps []

let ans2 =
    allSuspects
    |> Array.groupBy fst
    |> Array.map processGrp
    |> solve
    |> List.sortBy fst
    |> List.map snd
    |> (fun xs -> String.Join(",",xs))

ans2