#load "Helpers.fsx"

open System
open System.IO
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 24

type Direction =
    | NW | SW | W | E | NE | SE

let parseTile (s : string) =
    let sr = new StringReader(s)
    //let sb = StringBuilder()
    let mutable lst = List.empty

    while (sr.Peek() <> -1) do
        let char1 = sr.Read()
        let char2 = sr.Peek()
        //printfn "%c %c" (char char1) (char char2)
        //printfn "%i %i" (char1) (char2)
        //printfn "%A" lst
        if (char2 <> -1) then
            let res = 
                match char char1, char char2 with
                | 'n','w' -> sr.Read() |> ignore;NW
                | 's','w' -> sr.Read() |> ignore;SW
                | 'n','e' -> sr.Read() |> ignore;NE
                | 's','e' -> sr.Read() |> ignore;SE
                | 'e',_ -> E
                | 'w',_ -> W
            
            lst <- res :: lst
        else
            let res =
                match char char1 with
                | 'e' -> E
                | 'w' -> W
            lst <- res :: lst
                
    List.rev lst

let moveEven =
    function
    | NE -> (1,-1)
    | NW -> (0,-1)
    | E ->  (1,0)
    | W ->  (-1,0)
    | SE -> (1,1)
    | SW -> (0,1)

let moveOdd =
    function
    | NE -> (0,-1)
    | NW -> (-1,-1)
    | E ->  (1,0)
    | W ->  (-1,0)
    | SE -> (0,1)
    | SW -> (-1,1)


let tileCoord2 (lst : Direction list) =
    lst
    |> List.fold
        (fun (x,y) dir ->
            let (x',y') =
                if (y % 2 = 0) then
                    moveEven dir
                else
                    moveOdd dir
            (x+x',y+y'))
            (0,0)

let ans1 =
    data
    |> Array.map (parseTile >> tileCoord2)
    |> Array.groupBy id
    |> Array.map (fun (_,arr) -> Array.length arr)
    |> Array.filter (fun i -> i % 2 = 1)
    |> Array.length

ans1

/// Part 2

let blackTiles =
    data
    |> Array.map (parseTile >> tileCoord2)
    |> Array.groupBy id
    |> Array.filter (fun (_,arr) -> Array.length arr % 2 = 1)
    |> Array.map fst

let neighbors (x,y) =
    if (y % 2 = 0) then
        [| (x+1,y-1);(x+0,y-1);(x+1,y+0);(x-1,y+0);(x+1,y+1);(x+0,y+1)|]
    else
        [|(x+0,y-1);(x-1,y-1);(x+1,y+0);(x-1,y+0);(x+0,y+1);(x-1,y+1)|]

let solve2 blackTiles iters =
    let initState = Set.ofArray blackTiles

    let rec f state iters =
        //printfn "%i" (Set.count state)
        //printfn "%A" state

        if (iters = 0) then
            state
        else
            let candidates =
                let n = state |> Set.toArray |> Array.collect neighbors
                Array.concat [| (Set.toArray state); n|]
                |> Array.distinct

            let isBlackNextIter c =
                let isBlack = Set.contains c state
                let blackNeighbors =
                    neighbors c
                    |> Seq.filter (fun c -> Set.contains c state)
                    |> Seq.length
                match isBlack, blackNeighbors with
                | true, 0 -> false
                | true, x when x > 2 -> false
                | false, 2 -> true
                | _,_ -> isBlack

            let nextState = candidates |> Seq.filter isBlackNextIter |> Set.ofSeq
            f nextState (iters-1)

    f initState iters

let testData = """sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""

testData
|> split "\n"
|> Array.length

let blackTilesTest =
    testData
    |> split "\n"
    |> Array.map (parseTile >> tileCoord2)
    |> Array.groupBy id
    |> Array.filter (fun (_,arr) -> Array.length arr % 2 = 1)
    |> Array.map fst

blackTilesTest.Length

solve2 blackTilesTest 100 |> Set.count

let ans2 =
    solve2 blackTiles 100
    |> Set.count

ans2