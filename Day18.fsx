#load "Helpers.fsx"

open System
open System.Text.RegularExpressions
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 18

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

let rec solve2 s =
    let newStr =
        match s with
        | Regex "(\d+) \+ (\d+)" [d1;d2] ->
            let res = (int64 d1) + (int64 d2)
            s.Replace($"{d1} + {d2}", string res)
        | Regex "\((\d+)\)" [d1] ->
            s.Replace($"({d1})", string d1)
        | Regex "\((\d+) \* (\d+)\)" [d1;d2] ->
            let res = (int64 d1) * (int64 d2)
            s.Replace($"({d1} * {d2})", string res)
        | Regex "\((\d+) \* (\d+) \* (\d+)\)" [d1;d2;d3] ->
            let res = (int64 d1) * (int64 d2) * (int64 d3)
            s.Replace($"({d1} * {d2} * {d3})", string res)
        | Regex "\((\d+) \* (\d+) \* (\d+) \* (\d+)\)" [d1;d2;d3;d4] ->
            let res =
                (int64 d1) * (int64 d2) *
                (int64 d3) * (int64 d4)
            s.Replace($"({d1} * {d2} * {d3} * {d4})", string res)
        | Regex "\((\d+) \* (\d+) \* (\d+) \* (\d+) \* (\d+)\)" [d1;d2;d3;d4;d5] ->
            let res =
                (int64 d1) * (int64 d2) *
                (int64 d3) * (int64 d4) *
                (int64 d5)
            s.Replace($"({d1} * {d2} * {d3} * {d4} * {d5})", string res)
        | Regex "\((\d+) \* (\d+) \* (\d+) \* (\d+) \* (\d+) \* (\d+)\)" [d1;d2;d3;d4;d5;d6] ->
            let res =
                (int64 d1) * (int64 d2) *
                (int64 d3) * (int64 d4) *
                (int64 d5) * (int64 d6)
                
            s.Replace($"({d1} * {d2} * {d3} * {d4} * {d5} * {d6})", string res)
        | Regex "^(\d+) \* (\d+)$" [d1;d2] ->
            let res = (int64 d1) * (int64 d2)
            s.Replace($"{d1} * {d2}", string res)
        | Regex "^(\d+) \* (\d+) \* (\d+)$" [d1;d2;d3] ->
            let res = (int64 d1) * (int64 d2) * (int64 d3)
            s.Replace($"{d1} * {d2} * {d3}", string res)
        | Regex "^(\d+) \* (\d+) \* (\d+) \* (\d+)$" [d1;d2;d3;d4] ->
            let res =
                (int64 d1) * (int64 d2) *
                (int64 d3) * (int64 d4)
            s.Replace($"{d1} * {d2} * {d3} * {d4}", string res)
        | Regex "^(\d+) \* (\d+) \* (\d+) \* (\d+) \* (\d+)$" [d1;d2;d3;d4;d5] ->
            let res =
                (int64 d1) * (int64 d2) *
                (int64 d3) * (int64 d4) *
                (int64 d5)
            s.Replace($"{d1} * {d2} * {d3} * {d4} * {d5}", string res)
        | Regex "^(\d+) \* (\d+) \* (\d+) \* (\d+) \* (\d+) \* (\d+)$" [d1;d2;d3;d4;d5;d6] ->
            let res =
                (int64 d1) * (int64 d2) *
                (int64 d3) * (int64 d4) *
                (int64 d5) * (int64 d6)
            s.Replace($"{d1} * {d2} * {d3} * {d4} * {d5} * {d6}", string res)
        
        | _ -> s

    if (newStr <> s) then
        solve2 newStr
    else
        //printfn "%s" s
        bigint.Parse(s)

solve2 "1 + 2 * 3 + 4 * 5 + 6"

solve2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"

solve2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

solve2 "(2 * 11 * 5 * 8) * 6 + (6 * 7 * 12 * 28)"

let ans2 = data |> Array.sumBy solve2

ans2