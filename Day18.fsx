#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 18

let ex = "1 + (2 * 3) + (4 * (5 + 6))".ToCharArray() |> Array.filter ((<>)' ')

(*

Can meet:
Number -> use last operator seen
Operator -> Set as current operator
Left parens -> start recursive eval

Right parens -> return
EOF -> return

*)

let eval (expr : string) =
    let tokens = expr.ToCharArray() |> Array.filter ((<>)' ')
    
    let rec f idx (lhs : int64 option) (op : char option) =
        //printfn "%i: %A %A" idx lhs op
        match Array.tryItem idx tokens with
        | None
        | Some ')' -> idx, lhs
        | Some '(' ->
            let (endIdx,Some subEval) = f (idx+1) None None
            match lhs, op with
            | Some n, Some '+' -> f (endIdx+1) (Some (n+(int64 (string subEval)))) None
            | Some n, Some '*' -> f (endIdx+1) (Some (n*(int64 (string subEval)))) None
            | None, _ -> f (endIdx+1) (Some (int64 (string subEval))) None
        | Some '*' -> f (idx+1) lhs (Some '*')
        | Some '+' -> f (idx+1) lhs (Some '+')
        | Some d ->
            match lhs, op with
            | Some n, Some '+' -> f (idx+1) (Some (n+(int64 (string d)))) None
            | Some n, Some '*' -> f (idx+1) (Some (n*(int64 (string d)))) None
            | None, _ -> f (idx+1) (Some (int64 (string d))) None
    
    f 0 None None |> snd |> Option.get

eval "1 + (2 * 3) + (4 * (5 + 6))"

eval "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

let ans1 = data |> Array.sumBy eval

ans1

/// Part 2

let ans2 = data

ans2