#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 7

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseBagContents =
    function
    | "no other" -> None
    | Regex @"(\d+) (.+)" [ count; bag ] -> Some (int count,bag)

let map =
    data
    |> Array.map (fun s -> s.Split([|" bags contain "|], StringSplitOptions.None))
    |> Array.map (fun [|a1;a2|] -> a1, a2.Replace(" bags", "").Replace(" bag", "").Replace(".", "").Split([|", "|],StringSplitOptions.None))
    |> Array.map (fun (bag, contents) -> bag, contents |> Array.choose parseBagContents)
    |> Map.ofArray

let part1graph =
    map
    |> Map.toArray
    |> Array.collect (fun (bag,contents) -> contents |> Array.map (fun c -> (bag,snd c)))

let solve1 (data : (string*string) array) =
    let rec f data queue res =
        match queue with
        | [] -> res
        | x :: xs ->
            let bags =
                data
                |> Array.filter (snd >> (=)x)
                |> Array.map fst
                |> List.ofArray

            let newQueue = List.concat [bags; xs]
            let newRes = Set.union res (Set.ofList bags)
            f data newQueue newRes

    f data [ "shiny gold" ] Set.empty

let ans1 = solve1 part1graph |> Set.count

ans1

/// Part 2

let solve2 (data : (Map<string,(int*string) array>)) =
    let rec f start =
        data
        |> Map.find start
        |> Array.sumBy (fun (i,newBag) -> i + i * (f newBag))
    
    f "shiny gold"

let ans2 = solve2 map

ans2