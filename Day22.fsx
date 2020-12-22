#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let parseCard (s : string) =
    match Int32.TryParse(s) with
    | true, value -> Some value
    | false, _ -> None
    
let (deck1,deck2) =
    String.Join("\n", Helpers.Web.getInput 22 |> Array.tail)
    |> split "Player 2:"
    |> Array.map (split "\n")
    |> (fun [|arr1;arr2|] -> arr1 |> Array.choose parseCard |> List.ofArray, arr2 |> Array.choose parseCard |> List.ofArray)

let solve1 deck1 deck2 =
    let mutable i = 1
    let rec f top1 bottom1 top2 bottom2 =
        //printfn "--- Round %i ---" i
        //printfn "Top1: %A" top1
        //printfn "Bottom1: %A" bottom1
        //printfn "Top2: %A" top2
        //printfn "Bottom2: %A" bottom2
        //printfn ""
        //i <- i + 1
        //
        //if (i = 35) then failwithf ""
        match top1,bottom1,top2,bottom2 with
        | [], [], _, _ ->
            List.concat [ top2; List.rev bottom2 ]
        | _, _, [], [] ->
            List.concat [top1; List.rev bottom1 ]
        | [], xs1, [], xs2 ->
            f (List.rev xs1) [] (List.rev xs2) []
        | [], xs, _, _ -> f (List.rev xs) [] top2 bottom2
        | _, _, [], xs -> f top1 bottom1 (List.rev xs) []
        | x1 :: xs1, b1, x2::xs2, b2 when x1 > x2 ->
            f xs1 (x2 :: x1 :: b1) xs2 b2
        | x1 :: xs1, b1, x2::xs2, b2 when x2 > x1 ->
            f xs1 b1 xs2 (x1 :: x2 :: b2)
        | _ -> failwithf ""

    f deck1 [] deck2 []

solve1 [ 9; 2; 6; 3; 1 ] [ 5; 8; 4; 7; 10] 


let ans1 =
    solve1 deck1 deck2
    |> List.rev
    |> List.mapi (fun i x -> (int64 x) * (int64 (i+1)))
    |> List.reduce (+)

ans1

/// Part 2

let solve2 deck1 deck2 =
    let rec f i seen top1 bottom1 top2 bottom2 =
        //printfn "--- Round %i ---" i
        //printfn "Top1: %A" top1
        //printfn "Bottom1: %A" bottom1
        //printfn "Top2: %A" top2
        //printfn "Bottom2: %A" bottom2
        //printfn ""
        //
        //if (i = 35) then failwithf ""
        let trueDeck1 = List.concat [ top1; List.rev bottom1 ]
        let trueDeck2 = List.concat [ top2; List.rev bottom2 ]
        if (Set.contains (trueDeck1,trueDeck2) seen) then
            //printfn "Already seen this combo: %i" seen.Count
            1,[]
        else
            let len1 = top1.Length + bottom1.Length - 1
            let len2 = top2.Length + bottom2.Length - 1
            let newSeen = Set.add (trueDeck1,trueDeck2) seen
            
            match top1,bottom1,top2,bottom2 with
            | [], [], _, _ ->
                //seen <- Set.add (trueDeck1,trueDeck2) seen
                2,List.concat [ top2; List.rev bottom2 ]
            | _, _, [], [] ->
                //seen <- Set.add (trueDeck1,trueDeck2) seen
                1,List.concat [top1; List.rev bottom1 ]
            | [], xs1, [], xs2 ->
                f i seen (List.rev xs1) [] (List.rev xs2) []
            | [], xs, _, _ -> f i seen (List.rev xs) [] top2 bottom2
            | _, _, [], xs -> f i seen top1 bottom1 (List.rev xs) []
            
            | x1 :: xs1, b1, x2::xs2, b2 when len1 >= x1 && len2 >= x2 ->
                //printfn "Launching next game: %i, %i" len1 len2
                let (winner,_) =
                    let newDeck1 = trueDeck1.Tail |> List.take x1
                    let newDeck2 = trueDeck2.Tail |> List.take x2
                    f 1 newSeen newDeck1 [] newDeck2 []
                if (winner = 1) then
                    f (i+1) newSeen xs1 (x2 :: x1 :: b1) xs2 b2
                else
                    f (i+1) newSeen xs1 b1 xs2 (x1 :: x2 :: b2)
            | x1 :: xs1, b1, x2::xs2, b2 when x1 > x2 ->
                f (i+1) newSeen xs1 (x2 :: x1 :: b1) xs2 b2
            | x1 :: xs1, b1, x2::xs2, b2 when x2 > x1 ->
                f (i+1) newSeen xs1 b1 xs2 (x1 :: x2 :: b2)
            
    f 1 Set.empty deck1 [] deck2 []

solve2 [ 9; 2; 6; 3; 1 ] [ 5; 8; 4; 7; 10 ]

solve2 [ 43; 19 ] [ 2; 29; 14 ]

let solved = solve2 deck1 deck2

let ans2 =
    solved
    |> snd
    |> List.rev
    |> List.mapi (fun i x -> (int64 x) * (int64 (i+1)))
    |> List.reduce (+)

ans2