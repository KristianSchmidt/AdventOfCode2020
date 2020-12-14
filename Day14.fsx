#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Input = | Mask of string | Mem of int*string

let getMask (Mask s) = s

let convToBinary s = Convert.ToString(int s, 2).PadLeft(36, '0')

let binToInt s = Convert.ToInt64(s, 2)

let combine (mask : string) (value : string) =
    let maskArr = mask.ToCharArray()
    let valArr = value.ToCharArray()
    valArr
    |> Array.zip maskArr
    |> Array.map (function
                  | ('X', c) -> c
                  | (c, _) -> c
    )
    |> (fun arr -> String.Join("", arr))

let parseInput =
    function
    | Regex "mask = ([\dX]+)" [s] -> Mask s
    | Regex "mem\[(\d+)\] = (\d+)" [addr; value] -> Mem (int addr,convToBinary value)

let data =
    Helpers.Web.getInput 14
    |> Array.map parseInput

let solve (data : Input array) =
    let (initMask, input) = (Array.head data |> getMask, Array.tail data)

    let rec f rest mask map =
        match rest with
        | [] -> map
        | x :: xs ->
            match x with
            | Mask newMask -> f xs newMask map
            | Mem (addr,value) ->
                let newMap = map |> Map.add addr (combine mask value)
                f xs mask newMap

    f (List.ofArray input) initMask Map.empty

let ans1 =
    solve data
    |> Map.toArray
    |> Array.sumBy (snd >> binToInt) 

ans1

/// Part 2

type Input2 = | Mask of string | Mem2 of string*int

let parseInput2 =
    function
    | Regex "mask = ([\dX]+)" [s] -> Mask s
    | Regex "mem\[(\d+)\] = (\d+)" [addr; value] -> Mem2 (convToBinary addr,int value)

let data2 =
    Helpers.Web.getInput 14
    |> Array.map parseInput2

let replaceXs (addrX : char list) (zerosOnes : int list) =
    let sb = System.Text.StringBuilder()
    
    let rec f 
    
    
    ()

let combine2 (mask : string) (address : string) =
    let maskArr = mask.ToCharArray()
    let addrArr = address.ToCharArray()
    
    let addrX =
        addrArr
        |> Array.zip maskArr
        |> Array.map (function
                      | ('X', _) -> 'X'
                      | ('0', c) -> c
                      | ('1', _) -> '1'
        )
        |> (fun arr -> String.Join("", arr))

    let xCount = addrX.ToCharArray() |> Seq.filter ((=)'X') |> Seq.length
    let xCombinations = Helpers.product [0;1] xCount

    addrX

combine2 "00001X100000X0X0110XXX11001100X0X10X" "000000000000000000000000001111010111"

let ans2 = data

ans2