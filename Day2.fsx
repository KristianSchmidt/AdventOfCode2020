#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Rule = | Rule of int * int * char

let parseRule s =
    let m = Regex.Match(s, "(\d+)\-(\d+) ([a-z])")
    Rule ((int m.Groups.[1].Value), (int m.Groups.[2].Value), char m.Groups.[3].Value)

let data =
    Helpers.Web.getInput 2
    |> Array.map (Helpers.split ": ")
    |> Array.map (fun [|a1;a2|] -> parseRule a1, a2)

let isValid (Rule (lo, hi, c), passwd : string) =
    let count =
        passwd.ToCharArray()
        |> Seq.filter ((=)c)
        |> Seq.length
    lo <= count && count <= hi

let ans1 = data |> Array.filter isValid |> Array.length

ans1

/// Part 2

let isValid2 (Rule (lo, hi, c), passwd : string) = 
    //printfn "%i %i %c - %s" lo hi c passwd
    let chars = passwd.ToCharArray()
    let hasFirst = chars.[lo-1] = c
    let hasSecond = chars.[hi-1] = c
    hasFirst <> hasSecond
    
let ans2 = data |> Array.filter isValid2 |> Array.length

ans2