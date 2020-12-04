#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 4 |> List.ofArray

let groups =
    let rec loop p (grps : string list list) currGrp =
        match p with
        | x :: [] ->
            (x :: currGrp) :: grps
        | x :: xs ->
            match x with 
            | "" -> loop xs (currGrp :: grps) []
            | s -> loop xs grps (s :: currGrp)

    loop data [] []

let processedGroups =
    groups
    |> List.map (List.collect (fun s -> s.Split(' ') |> Array.toList))

let sets =
    processedGroups
    |> List.map (List.map (fun s -> s.Split(':') |> Array.head))
    |> List.map Set.ofList

let containsAlls (hs : Set<string>) =
    Set.contains "byr" hs &&
    Set.contains "iyr" hs &&
    Set.contains "eyr" hs &&
    Set.contains "hgt" hs &&
    Set.contains "hcl" hs &&
    Set.contains "ecl" hs &&
    Set.contains "pid" hs
    

let ans1 =
    sets
    |> List.filter containsAlls
    |> List.length

ans1

/// Part 2

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let maps =
    processedGroups
    |> List.map (List.map (fun s -> (s.Split(':') |> Array.head, s.Split(':').[1])))
    |> List.map Map.ofList

let validByr i = i >= 1920 && i <= 2002
let validIyr i = i >= 2010 && i <= 2020
let validEyr i = i >= 2020 && i <= 2030

let validHgt (s : string) =
    match s with 
    | Regex @"(\d{2,3})(cm|in)" [ num; un ] ->
        match un with
        | "in" when (int num) >= 59 && (int num) <= 76 -> true
        | "cm" when (int num) >= 150 && (int num) <= 193 ->
            true
        | _ -> false
    | _ -> false

let validHcl (s : string) =
    match s with
    | Regex @"#[0-9a-f]{6}" [] -> true
    | _ -> false

let validEcl (s : string) =
    match s with
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
    | _ -> false

let validPid (s : string) =
    match s with
    | Regex @"^\d{9}$" [] -> true
    | _ -> false

let validPassport (m : Map<string,string>) =
    let isValidByr =
        m
        |> Map.tryFind "byr"
        |> Option.map (int >> validByr)
        |> Option.defaultValue false

    let isValidIyr =
        m
        |> Map.tryFind "iyr"
        |> Option.map (int >> validIyr)
        |> Option.defaultValue false

    let isValidEyr =
        m
        |> Map.tryFind "eyr"
        |> Option.map (int >> validEyr)
        |> Option.defaultValue false

    let isValidHgt =
        m
        |> Map.tryFind "hgt"
        |> Option.map (validHgt)
        |> Option.defaultValue false

    let isValidHcl =
        m
        |> Map.tryFind "hcl"
        |> Option.map (validHcl)
        |> Option.defaultValue false
    
    let isValidEcl =
        m
        |> Map.tryFind "ecl"
        |> Option.map (validEcl)
        |> Option.defaultValue false

    let isValidPid =
        m
        |> Map.tryFind "pid"
        |> Option.map (validPid)
        |> Option.defaultValue false

    isValidByr && isValidIyr && isValidEyr &&
        isValidHgt && isValidHcl && isValidEcl &&
        isValidPid

let ans2 =
    maps 
    |> List.filter validPassport
    |> List.length
    

maps
|> List.filter validPassport
|> List.choose (Map.tryFind "pid")

|> List.map (fun s -> s.Length)
|> List.distinct

maps.[0]

ans2