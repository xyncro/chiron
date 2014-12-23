namespace Chiron

open System
open Aether
open Aether.Operators

    
[<AutoOpen>]
module internal Lenses = 

    let decimalPLens =
        isoJNumberPLens 
        >?-> isoLens decimal float

    let intPLens =
        isoJNumberPLens 
        >?-> isoLens int float

    let int16PLens =
        isoJNumberPLens 
        >?-> isoLens int16 float

    let int64PLens =
        isoJNumberPLens 
        >?-> isoLens int64 float

    let guidPLens =
        isoJStringPLens 
        >??> isoPLens (fun s -> 
            Guid.TryParse s 
            |> function | true, v -> Some v | _ -> None) string

    let uint16PLens =
        isoJNumberPLens 
        >?-> isoLens uint16 float

    let uint32PLens =
        isoJNumberPLens 
        >?-> isoLens uint32 float

    let uint64PLens =
        isoJNumberPLens 
        >?-> isoLens uint64 float


[<AutoOpen>]
module Mapping =

    type Defaults = Defaults with
        
        // bool

        static member inline fromJSON (_: bool) =
            getPLM isoJBoolPLens

        static member inline toJSON (x: bool) =            
            setPLM isoJBoolPLens x

        // decimal

        static member inline fromJSON (_: decimal) = 
            getPLM decimalPLens

        static member inline toJSON (x: decimal) = 
            setPLM decimalPLens x

        // float

        static member inline fromJSON (_: float) =
            getPLM isoJNumberPLens

        static member inline toJSON (x: float) =
            setPLM isoJNumberPLens x

        // int

        static member inline fromJSON (_: int) = 
            getPLM intPLens

        static member inline toJSON (x: int) = 
            setPLM intPLens x

        // int16

        static member inline fromJSON (_: int16) = 
            getPLM int16PLens

        static member inline toJSON (x: int16) = 
            setPLM int16PLens x

        // int64

        static member inline fromJSON (_: int64) = 
            getPLM int64PLens

        static member inline toJSON (x: int64) = 
            setPLM int64PLens x

        // guid

        static member inline fromJSON (_: Guid) =
            getPLM guidPLens

        static member inline toJSON (x: Guid) =
            setPLM guidPLens x

        // string

        static member inline fromJSON (_: string) =
            getPLM isoJStringPLens

        static member inline toJSON (x: string) =
            setPLM isoJStringPLens x
            
        // uint16

        static member inline fromJSON (_: uint16) = 
            getPLM uint16PLens

        static member inline toJSON (x: uint16) = 
            setPLM uint16PLens x

        // uint32

        static member inline fromJSON (_: uint32) = 
            getPLM uint32PLens

        static member inline toJSON (x: uint32) = 
            setPLM uint32PLens x

        // uint64

        static member inline fromJSON (_: uint64) = 
            getPLM uint64PLens

        static member inline toJSON (x: uint64) = 
            setPLM uint64PLens x


    let inline internal fromJSONWithDefaults (a: ^a, b: ^b) =
        ((^a or ^b) : (static member fromJSON: ^b -> ^b JSONFunc) b)

    let inline internal toJSONWithDefaults (a: ^a, b: ^b) =
        ((^a or ^b) : (static member toJSON: ^b -> unit JSONFunc) b)

    let inline fromJSON (x: JSON) : Choice<'a, string> = 
        fromJSONWithDefaults (Defaults, Unchecked.defaultof<'a>) x |> fst

    let inline toJSON (x: 'a) : JSON = 
        toJSONWithDefaults (Defaults, x) (JObject (Map.empty)) |> snd

    let inline getJSON k =
        json {
            let! x = getPLM (isoJObjectPLens >??> mapPLens k)

            match fromJSON x with
            | Choice1Of2 x -> return! choice1 x
            | Choice2Of2 x -> return! choice2 x }

    let inline setJSON k v =
        json {
            do! setPLM (isoJObjectPLens >??> mapPLens k) (toJSON v) }
                    

    type Defaults with

        // option

        static member inline fromJSON (_: 'a option) =
            json {
                let! json = getM

                match json with
                | JNull -> 
                    return! choice1 None
                | _ ->
                    match fromJSON json with
                    | Choice1Of2 (a: 'a) -> return! choice1 (Some a)
                    | Choice2Of2 s -> return! choice2 s }

        static member inline toJSON (x: 'a option) =
            json {
                match x with
                | Some a -> do! (fun _ -> Choice1Of2 (), toJSON a)
                | _ -> do! setM JNull }


    type Defaults with

        // list

        static member inline fromJSON (_: 'a list) =
            json {
                let! json = getM

                match json with
                | JArray xs ->
                    let (xs: 'a list, errs: string list) = 
                        xs 
                        |> List.map fromJSON
                        |> List.partition (function | Choice1Of2 _ -> true | _ -> false)
                        |> (fun (xs, errs) ->
                            xs |> List.choose (function | Choice1Of2 x -> Some x | _ -> None),
                            errs |> List.choose (function | Choice2Of2 x -> Some x | _ -> None))
                        
                    match xs, errs with
                    | _, h :: _ -> return! choice2 h
                    | xs, _ -> return! choice1 xs
                | _ ->
                    return! choice2 "Not a JArray" }

        static member inline toJSON (x: 'a list) =
            json {
                do! (fun _ -> Choice1Of2 (), JArray (List.map toJSON x)) }


module Operators =

    let inline (!!) k =
        getJSON k

    let inline (<!) k v =
        setJSON k v
