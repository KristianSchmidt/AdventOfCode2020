#load "Helpers.fsx"

open System
open Helpers
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let rotate grid =
    let height, width = Array2D.length1 grid, Array2D.length2 grid
    Array2D.init width height (fun row column -> Array2D.get grid (height - column - 1) row)

let flipH grid =
    let height, width = Array2D.length1 grid, Array2D.length2 grid
    Array2D.init width height (fun row column -> Array2D.get grid (height-row-1) column)

let flipV grid =
    let height, width = Array2D.length1 grid, Array2D.length2 grid
    Array2D.init width height (fun row column -> Array2D.get grid row (width-column-1))

let allCombinations grid =
    let allRotations = [|id; rotate; rotate>>rotate; rotate>>rotate>>rotate|]
    let allFlips = [|id;flipH;flipV; (flipH>>flipV)|]
    allRotations
    |> Array.collect (fun rot -> allFlips |> Array.map (fun f -> f (rot grid)))

let testInput = """
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
"""

let data =
    Helpers.Web.getInput 20
    //testInput |> split "\n"
    |> (fun arr -> String.Join("\n",arr))
    |> (fun s -> Regex.Split(s, "Tile "))
    |> Array.tail
    |> Array.map
        (fun s -> match s with
                  | Regex "(\d+):\n" [tileId] -> (int64 tileId,s.Substring(tileId.Length+2).Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries))
        )
    
let rev (s : string) =
    s.ToCharArray() |> Array.rev |> (fun arr -> String.Join("",arr))

let possibles (arr : string array) =
    let len = arr.Length
    let top = arr.[0]
    let bottom = arr.[len-1]
    let right =
        arr
        |> Array.map (fun s -> s.Substring(len-1))
        |> (fun arr -> String.Join("", arr))
    let left =
        arr
        |> Array.map (fun s -> s.Substring(0,1))
        |> (fun arr -> String.Join("", arr))
    [| top;bottom;right;left
       rev top; rev bottom; rev right; rev left |]
    |> Set.ofArray

let possiblesMap =
    data
    |> Array.map (fun (id,arr) -> id,possibles arr)
    |> Map.ofArray

let intersection (id1, id2) =
    let set1 = possiblesMap |> Map.find id1
    let set2 = possiblesMap |> Map.find id2
    Set.intersect set1 set2

let couldMatch id1 id2 =
    intersection (id1, id2) |> Set.isEmpty |> not

let keys = data |> Array.map fst

let matches =
    seq {
        for idx1 in 0..(keys.Length-1) do
            for idx2 in (idx1+1)..(keys.Length-1) do
                let key1 = keys.[idx1]
                let key2 = keys.[idx2]
                if (key1 <> key2 && couldMatch key1 key2) then
                    yield (key1,key2)
    } |> Seq.toArray

let matchesWith key =
    matches
    |> Array.filter (fun (e1,e2) -> e1 = key || e2 = key)
    |> Array.map (fun (e1,e2) -> if (e1 = key) then e2 else e1)

let numMatches key =
    matchesWith key
    |> Array.length

let ans1 =
    keys
    |> Array.map (fun k -> k,numMatches k)
    |> Array.sortBy snd
    |> Array.filter (snd >> ((=)2))
    |> Array.map fst
    |> Array.reduce (*)

ans1

/// Part 2
    
let bottomLeft =
    keys
    |> Array.map (fun k -> k,numMatches k)
    |> Array.sortBy snd
    |> Array.head
    |> fst

let grid = Array2D.init 12 12 (fun _ _ -> -1L)

grid.[11,0] <- bottomLeft

let [|besides;over|] = matchesWith bottomLeft

grid.[10,0] <- over
grid.[11,1] <- besides

let onlyMatch alreadySeen (x,y) =
    let (underX,underY) = (x+1,y)
    let (leftX,leftY) = (x,y-1)
    if (x = 11) then
        matchesWith grid.[leftX,leftY]
        |> Array.filter (fun e -> not <| Set.contains e alreadySeen)
        |> Seq.exactlyOne
    else if (y = 0) then
        matchesWith grid.[underX, underY]
        |> Array.filter (fun e -> not <| Set.contains e alreadySeen)
        |> Seq.exactlyOne
    else
        let underSet = Set.ofArray <| (matchesWith grid.[underX,underY] |> Array.filter (fun e -> not <| Set.contains e alreadySeen))
        let leftSet  = Set.ofArray <| (matchesWith grid.[leftX,leftY] |> Array.filter (fun e -> not <| Set.contains e alreadySeen))
        Set.intersect underSet leftSet
        |> Seq.exactlyOne

let mutable visited = Set.ofArray [|bottomLeft;besides;over|]

for rowNum in [|9..-1..-11|] do
    let toVisit =
        [| rowNum+1..11 |]
        |> Array.mapi (fun i x -> x,i+1)
        |> (fun arr -> Array.append arr [|(rowNum,0)|])
        |> Array.filter (fun (x,y) -> x >= 0 && y >= 0 && x <= 11 && y <= 11)

    for (x,y) in toVisit do
        //printfn "Trying %i,%i" x y
        let res = onlyMatch visited (x,y)
        grid.[x,y] <- res
        visited <- Set.add res visited

let rawMap = Map.ofArray data

let toGrid (arr : string array) =
    arr
    |> Array.map (fun s -> s.ToCharArray())
    |> array2D

let fromGrid (grid : char [,]) =
    [|0..(Array2D.length1 grid)-1|]
    |> Array.map (fun i -> String(grid.[i,*]))
    
let matchWithIdBottom (rawData : string array) matchId =
    let matchRaw = Map.find matchId rawMap
    let grid = toGrid matchRaw

    let topOfPrev = rawData.[0]

    let getBottom (arr : string array) = arr.[arr.Length-1]
    
    let distincts =
        allCombinations grid
        |> Array.filter (fromGrid >> getBottom >> ((=)topOfPrev))
        |> Array.distinct

    printfn "Distincts %i: %i" matchId distincts.Length
    
    fromGrid distincts.[0]

let matchWithIdLeft (rawData : string array) matchId =
    let matchRaw = Map.find matchId rawMap
    let grid = toGrid matchRaw

    let rightOfPrev = String(rawData |> Array.map (fun a -> a.[a.Length-1]))
    
    let getLeft (arr : string array) = String(arr |> Array.map (fun a -> a.[0]))
    
    let distincts =
        allCombinations grid
        |> Array.filter (fromGrid >> getLeft >> ((=)rightOfPrev))
        |> Array.distinct

    printfn "Distincts %i: %i" matchId distincts.Length
    
    fromGrid distincts.[0]

let matchWithId (rawDataBottom : string array) (rawDataLeft : string array) matchId =
    let matchRaw = Map.find matchId rawMap
    let grid = toGrid matchRaw

    let topOfPrev = rawDataBottom.[0]
    let rightOfPrev = String(rawDataLeft |> Array.map (fun a -> a.[a.Length-1]))

    let getLeft (arr : string array) = String(arr |> Array.map (fun a -> a.[0]))
    let getBottom (arr : string array) = arr.[arr.Length-1]

    let distincts =
        allCombinations grid
        |> Array.filter (fromGrid >> getLeft >> ((=)rightOfPrev))
        |> Array.filter (fromGrid >> getBottom >> ((=)topOfPrev))
        |> Array.distinct

    printfn "Distincts %i: %i" matchId distincts.Length
    
    fromGrid distincts.[0]

let finalGrid : string [] [,] = Array2D.init 12 12 (fun _ _ -> Array.empty)

finalGrid.[11,0] <- Map.find bottomLeft rawMap

for rowNum in [|10..-1..-11|] do
    let toVisit =
        [| rowNum+1..11 |]
        |> Array.mapi (fun i x -> x,i+1)
        |> (fun arr -> Array.append arr [|(rowNum,0)|])
        |> Array.filter (fun (x,y) -> x >= 0 && y >= 0 && x <= 11 && y <= 11)

    for (x,y) in toVisit do
        printfn "Trying %i,%i" x y
        let res =
            if (y = 0) then
                matchWithIdBottom finalGrid.[x+1,y] grid.[x,y]
            else if (x = 11) then
                matchWithIdLeft finalGrid.[x,y-1] grid.[x,y]
            else
                matchWithId finalGrid.[x+1,y] finalGrid.[x,y-1] grid.[x,y]

        finalGrid.[x,y] <- res

let removeBorder (arr : string array) =
    arr.[1..arr.Length-2]
    |> Array.map (fun a -> a.[1..a.Length-2])

let noBorders = finalGrid |> Array2D.map removeBorder

let collectRow i =
    let rowSlice = noBorders.[i,*]
    //printfn "Dim1: %i" rowSlice.Length
    //printfn "Dim2: %i" rowSlice.[0].Length
    [|0..7|]
    |> Array.map (fun j -> 
        rowSlice
        |> Array.map (fun arr -> arr.[j])
        |> (fun arr -> String.Join("",arr))
    )

let pictureGrid =
    [|0..11|]
    |> Array.collect collectRow
    |> toGrid

let allPictures = allCombinations pictureGrid

let checkAt (grid : char[,]) (x,y) =
    let otherPoints =
        [| (x+1,y-18);(x+1,y-13);(x+1,y-12);(x+1,y-7)
           (x+1,y-6);(x+1,y-1);(x+1,y);(x+1,y+1);(x+2,y-17)
           (x+2,y-14);(x+2,y-11);(x+2,y-8);(x+2,y-5);(x+2,y-2)|]

    let allWithinBorders =
        otherPoints
        |> Array.forall (fun (x,y) -> x >= 0 && y >= 0 && x <= 95 && y <= 95)

    if (not allWithinBorders) then
        None
    else
        let allAreMonsters = otherPoints |> Array.forall (fun (x,y) -> grid.[x,y] = '#')
        if (allAreMonsters) then
            Some (Array.append [|(x,y)|] otherPoints)
        else
            None

let checkForSeaMonster (grid : char [,]) =
    let len1 = Array2D.length1 grid
    let len2 = Array2D.length2 grid
    let possiblePoints =
        seq {
            for i in 0..len1-1 do
                for j in 0..len2-1 do
                    if (grid.[i,j] = '#') then
                        yield (i,j)
        } |> Seq.toArray

    possiblePoints
    |> Array.map (checkAt grid)

let hasNotNone (opt : 'a option array) =
    opt |> Array.exists Option.isSome

let ans2 =
    let totalCount =
        fromGrid pictureGrid
        |> Array.sumBy (fun s -> s.ToCharArray() |> Array.filter ((=)'#') |> Array.length)
    
    let seaMonsterCount =
        allPictures
        |> Array.map checkForSeaMonster
        |> Array.find hasNotNone
        |> Array.choose id
        |> Array.collect id
        |> Array.distinct
        |> Array.length
    
    totalCount - seaMonsterCount   


ans2