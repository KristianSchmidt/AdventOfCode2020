#load "Helpers.fsx"

open System
open System.Text.RegularExpressions
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (rules,data) =
    let arr = Helpers.Web.getInput 19
    let idx = arr |> Array.findIndex ((=)"")
    arr.[0..idx-1], arr.[idx+1..]

rules |> Array.iter (printfn "%s")

type Rule =
    | OrRule of rule1 : Rule * rule2 : Rule
    | Or of rule1 : int * rule2 : int
    | And of rule1 : int * rule2 : int
    | Letter of string
    | Alias of rule : int

let parseRule (s : string) =
    match s with
    | Regex "^(\d+): (\d+)$" [idx;alias] ->
        (int idx), Alias (int alias)
    | Regex "^(\d+): (\d+) (\d+)$" [idx;rule1;rule2] ->
        (int idx), And ((int rule1),(int rule2))
    | Regex "^(\d+): (\d+) \| (\d+)$" [idx;rule1;rule2] ->
        (int idx), Or ((int rule1),(int rule2))
    | Regex "^(\d+): (\d+) (\d+) \| (\d+) (\d+)$" [idx;p11;p12;p21;p22] ->
        (int idx), OrRule (And (int p11,int p12),And (int p21,int p22))
    | Regex "^(\d+): \"(.)\"$" [idx;letter] ->
        (int idx), Letter letter

let ruleMap =
    rules
    |> Array.map parseRule
    |> Map.ofArray

let ruleResults : (string option) array = Array.init ruleMap.Count (fun _ -> None)

let rec renderRule i =
    match ruleResults.[i] with
    | Some s -> s
    | None ->
        let rule = ruleMap |> Map.find i
        let res =
            match rule with
            | Letter s -> s
            | And (rule1,rule2) ->
                let res1 = renderRule rule1
                let res2 = renderRule rule2
                res1+res2
            | Or (rule1,rule2) ->
                let res1 = renderRule rule1
                let res2 = renderRule rule2
                $"({res1}|{res2})"
            | Alias rule ->
                renderRule rule
            | OrRule (And(rule11,rule12),And(rule21,rule22)) ->
                let res1 = (renderRule rule11) + (renderRule rule12)
                let res2 = (renderRule rule21) + (renderRule rule22)
                $"({res1}|{res2})"
            | _ -> failwithf "Could not match on %A" rule
        ruleResults.[i] <- Some res
        res

let regex = Regex($"^{renderRule 0}$")

let ans1 =
    data
    |> Array.filter (fun s -> regex.Match(s).Success)
    |> Array.length

ans1

/// Part 2

let ruleResults2 : (string option) array = Array.init ruleMap.Count (fun _ -> None)

let rec renderRule2 i =
    if (i = 8) then
        let rule42 = renderRule2 42
        $"(({rule42})+)"
    else if (i = 11) then
        let rule42 = renderRule2 42
        let rule31 = renderRule2 31
        let baseCase = $"{rule42}{rule31}"
        let case2 = $"{rule42}{rule42}{rule31}{rule31}"
        let case3 = $"{rule42}{rule42}{rule42}{rule31}{rule31}{rule31}"
        let case4 = $"{rule42}{rule42}{rule42}{rule42}{rule31}{rule31}{rule31}{rule31}"
        let case5 = $"{rule42}{rule42}{rule42}{rule42}{rule42}{rule31}{rule31}{rule31}{rule31}{rule31}"
        
        $"({baseCase}|{case2}|{case3}|{case4}|{case5})"
    else
        match ruleResults2.[i] with
        | Some s -> s
        | None ->
            let rule = ruleMap |> Map.find i
            let res =
                match rule with
                | Letter s -> s
                | And (rule1,rule2) ->
                    let res1 = renderRule2 rule1
                    let res2 = renderRule2 rule2
                    res1+res2
                | Or (rule1,rule2) ->
                    let res1 = renderRule2 rule1
                    let res2 = renderRule2 rule2
                    $"({res1}|{res2})"
                | Alias rule ->
                    renderRule2 rule
                | OrRule (And(rule11,rule12),And(rule21,rule22)) ->
                    let res1 = (renderRule2 rule11) + (renderRule2 rule12)
                    let res2 = (renderRule2 rule21) + (renderRule2 rule22)
                    $"({res1}|{res2})"
                | _ -> failwithf "Could not match on %A" rule
            ruleResults.[i] <- Some res
            res

let regex2 = Regex($"^{renderRule2 0}$")

let ans2 =
    data
    |> Array.filter (fun s -> regex2.Match(s).Success)
    |> Array.length

ans2