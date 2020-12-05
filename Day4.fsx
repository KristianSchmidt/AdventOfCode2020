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

let maps =
    groups
    |> List.map (List.collect (fun s -> s.Split(' ') |> Array.toList))
    |> List.map (List.map (fun s -> (s.Split(':') |> Array.head, s.Split(':').[1])))
    |> List.map Map.ofList

let mapHasKey map key = map |> Map.tryFind key |> Option.isSome

let containsAllKeys (map : Map<string,string>) =
    mapHasKey map "byr" &&
    mapHasKey map "iyr" &&
    mapHasKey map "eyr" &&
    mapHasKey map "hgt" &&
    mapHasKey map "hcl" &&
    mapHasKey map "ecl" &&
    mapHasKey map "pid"
    
let ans1 =
    maps
    |> List.filter containsAllKeys
    |> List.length

ans1

/// Part 2

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let fulfillsRegex regex = function | Regex regex [] -> true | _ -> false

let validByr (s : string) = int s >= 1920 && int s <= 2002
let validIyr (s : string) = int s >= 2010 && int s <= 2020
let validEyr (s : string) = int s >= 2020 && int s <= 2030

let validHgt (s : string) =
    match s with 
    | Regex @"(\d{2,3})(cm|in)" [ num; un ] ->
        match un with
        | "in" when (int num) >= 59 && (int num) <= 76 -> true
        | "cm" when (int num) >= 150 && (int num) <= 193 -> true
        | _ -> false
    | _ -> false

let validHcl = fulfillsRegex @"#[0-9a-f]{6}"

let validEcl (s : string) =
    match s with
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
    | _ -> false

let validPid = fulfillsRegex @"^\d{9}$"

let validateField map field f =
    map
    |> Map.tryFind field
    |> Option.map f
    |> Option.defaultValue false

let validPassport (m : Map<string,string>) =
    [ validateField m "byr" validByr
      validateField m "iyr" validIyr
      validateField m "eyr" validEyr
      validateField m "hgt" validHgt
      validateField m "hcl" validHcl
      validateField m "ecl" validEcl
      validateField m "pid" validPid ]
    |> List.forall id

let ans2 =
    maps 
    |> List.filter validPassport
    |> List.length
    
ans2