namespace ChironB.Benchmarks

open Chiron
open BenchmarkDotNet.Attributes
open System.Text

module DecodeExamples =
    module E = Chiron.Serialization.Json.Encode
    let testJson =
        JsonObject.ofPropertyList
            [ "2", E.bool true
              "3", E.int 42 ]
        |> JsonObject.optimizeRead
        |> E.jsonObject
    module Inline =
        module Explicit =
            module ComputationExpression =
                module D = Chiron.Serialization.Json.Decode
                module JO = Chiron.JsonObject
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                    static member FromJson (_:Testing): JsonReader<Testing> = jsonReader {
                        let! o = JO.readOptionalWith D.int "1"
                        let! t = JO.readWith D.bool "2"
                        let! r = JO.readWith D.int "3"
                        return { one = o; two = t; three = r }
                    }

            module Operators =
                module D = Chiron.Serialization.Json.Decode
                module JO = Chiron.JsonObject
                open Chiron.ObjectReader.Operators
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                    static member Decode (jObj: JsonObject): JsonResult<Testing> =
                        ((fun o t r -> { one = o; two = t; three = r })
                        <!> JO.readOptionalWith D.int "1"
                        <*> JO.readWith D.bool "2"
                        <*> JO.readWith D.int "3") jObj
                    static member FromJson (_: Testing): JsonReader<Testing> =
                        let decode jObj = Testing.Decode(jObj)
                        ObjectReader.toJsonReader decode

        module Inferred =
            module ComputationExpression =
                module D = Chiron.Serialization.Json.Decode
                module JO = Chiron.JsonObject
                module JO = Chiron.Inference.JsonObject
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                    static member FromJson (_:Testing): JsonReader<Testing> = jsonReader {
                        let! o = JO.readOptional "1"
                        let! t = JO.read "2"
                        let! r = JO.read "3"
                        return { one = o; two = t; three = r }
                    }

            module Operators =
                module D = Chiron.Serialization.Json.Decode
                module JO = Chiron.JsonObject
                module JO = Chiron.Inference.JsonObject
                open Chiron.ObjectReader.Operators
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                    static member Decode (jObj: JsonObject): JsonResult<Testing> =
                        ((fun o t r -> { one = o; two = t; three = r })
                        <!> JO.readOptional "1"
                        <*> JO.read "2"
                        <*> JO.read "3") jObj
                    static member FromJson (_: Testing): JsonReader<Testing> =
                        let decode jObj = Testing.Decode(jObj)
                        ObjectReader.toJsonReader decode

    module InModule =
        module Explicit =
            module ComputationExpression =
                module D = Chiron.Serialization.Json.Decode
                module JO = Chiron.JsonObject
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                module Testing =
                    let toJson = jsonReader {
                        let! o = JO.readOptionalWith D.int "1"
                        let! t = JO.readWith D.bool "2"
                        let! r = JO.readWith D.int "3"
                        return { one = o; two = t; three = r }
                    }
                type Testing with
                    static member FromJson (_: Testing): JsonReader<Testing> =
                        Testing.toJson

            module Operators =
                module D = Chiron.Serialization.Json.Decode
                module JO = Chiron.JsonObject
                open Chiron.ObjectReader.Operators
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                module Testing =
                    let decode =
                        (fun o t r -> { one = o; two = t; three = r })
                        <!> JO.readOptionalWith D.int "1"
                        <*> JO.readWith D.bool "2"
                        <*> JO.readWith D.int "3"
                    let toJson =
                        ObjectReader.toJsonReader decode
                type Testing with
                    static member FromJson (_: Testing): JsonReader<Testing> =
                        Testing.toJson

        module Inferred =
            module ComputationExpression =
                module D = Chiron.Serialization.Json.Decode
                module JO = Chiron.JsonObject
                module JO = Chiron.Inference.JsonObject
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                module Testing =
                    let toJson = jsonReader {
                        let! o = JO.readOptional "1"
                        let! t = JO.read "2"
                        let! r = JO.read "3"
                        return { one = o; two = t; three = r }
                    }
                type Testing with
                    static member FromJson (_: Testing): JsonReader<Testing> =
                        Testing.toJson

            module Operators =
                module D = Chiron.Serialization.Json.Decode
                module JO = Chiron.JsonObject
                module JO = Chiron.Inference.JsonObject
                open Chiron.ObjectReader.Operators
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                module Testing =
                    let decode =
                        (fun o t r -> { one = o; two = t; three = r })
                        <!> JO.readOptional "1"
                        <*> JO.read "2"
                        <*> JO.read "3"
                    let toJson =
                        ObjectReader.toJsonReader decode
                type Testing with
                    static member FromJson (_: Testing): JsonReader<Testing> =
                        Testing.toJson

    module Obsolete =
        open ChironObsolete

        let testJson =
            Json.Object <| Map.ofList
                [ "2", Json.Bool true
                  "3", Json.Number 42M ]

        module ComputationExpression =
            type Testing =
                { one: int option
                  two: bool
                  three: int }
                static member FromJson (_: Testing): Json<Testing> = json {
                    let! o = Json.readOrDefault "1" None
                    let! t = Json.read "2"
                    let! r = Json.read "3"
                    return { one = o; two = t; three = r }
                }

        module Operators =
            open ChironObsolete.Operators
            type Testing =
                { one: int option
                  two: bool
                  three: int }
                static member FromJson (_: Testing): Json<Testing> =
                    (fun o t r -> { one = o; two = t; three = r })
                    <!> Json.readOrDefault "1" None
                    <*> Json.read "2"
                    <*> Json.read "3"

[<Config(typeof<CoreConfig>)>]
type Decoding () =
    [<Benchmark>]
    member x.Inline_Explicit_ComputationExpression () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.Inline.Explicit.ComputationExpression.Testing>) |> ignore

    [<Benchmark>]
    member x.InModule_Explicit_ComputationExpression () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.InModule.Explicit.ComputationExpression.Testing>) |> ignore

    [<Benchmark>]
    member x.Inline_Inferred_ComputationExpression () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.Inline.Inferred.ComputationExpression.Testing>) |> ignore

    [<Benchmark>]
    member x.InModule_Inferred_ComputationExpression () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.InModule.Inferred.ComputationExpression.Testing>) |> ignore

    [<Benchmark>]
    member x.Inline_Explicit_Operators () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.Inline.Explicit.Operators.Testing>) |> ignore

    [<Benchmark>]
    member x.InModule_Explicit_Operators () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.InModule.Explicit.Operators.Testing>) |> ignore

    [<Benchmark>]
    member x.Inline_Inferred_Operators () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.Inline.Inferred.Operators.Testing>) |> ignore

    [<Benchmark>]
    member x.InModule_Inferred_Operators () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.InModule.Inferred.Operators.Testing>) |> ignore

    [<Benchmark(Baseline=true)>]
    member x.Version6_ComputationExpression () =
        (ChironObsolete.Mapping.Json.deserialize DecodeExamples.Obsolete.testJson : DecodeExamples.Obsolete.ComputationExpression.Testing) |> ignore

    [<Benchmark>]
    member x.Version6_Operators () =
        (ChironObsolete.Mapping.Json.deserialize DecodeExamples.Obsolete.testJson : DecodeExamples.Obsolete.Operators.Testing) |> ignore

module E = Chiron.Serialization.Json.Encode
module D = Chiron.Serialization.Json.Decode

module ListDecoders =
    let inline (>=>) decodeA decodeB = JsonResult.Operators.(>=>) decodeA decodeB
    let inline (>->) decode convert = JsonResult.Operators.(>->) decode convert

    let listInner  (decode: JsonReader<'a>) (init: 's) (fold: 'a -> 's -> 's) : Json list -> JsonResult<'s> =
        let folder json (sR, i) =
            let aR = JsonResult.withIndexTag i decode json
            let next =
                match sR, aR with
                | Ok s, Ok a -> Ok (fold a s)
                | Error errs, Error [err] -> Error (err :: errs)
                | Error err1, Error err2 -> Error (err2 @ err1)
                | Error errs, _
                | _, Error errs -> Error errs
            (next, i + 1u)
        fun xs ->
            List.foldBack folder xs (Ok init, 0u) |> fst

    let fstλ<'a,'b> : ('a * 'b) -> 'a = (do ()); fun (a, _) -> a

    let arrayFolder x (arr: 'a array, idx) =
        arr.[idx] <- x
        (arr, idx - 1)

    let jsonListToArray inner decode =
        let mapper = fstλ
        fun jLst ->
            let len = List.length jLst
            inner decode (Array.zeroCreate len, len - 1) arrayFolder jLst
            |> JsonResult.map mapper

    let arrayWith (decode: JsonReader<'a>) : JsonReader<'a array> =
        D.list >=> jsonListToArray listInner decode

    let arrayWithAlt (decode: JsonReader<'a>) : JsonReader<'a[]> =
        let rec loop (aggR: JsonResult<'a[]>) idx lastX xs =
            let xR = JsonResult.withIndexTag idx decode lastX
            match aggR, xR with
            | JsonResult.Ok agg, JsonResult.Ok x ->
                agg.[int idx] <- x
                match xs with
                | [] -> JsonResult.Ok agg
                | nextX::nextXs -> loop aggR (idx + 1u) nextX nextXs
            | JsonResult.Error errs1, JsonResult.Error errs2 ->
                let nextAggR = JsonResult.Error (errs1 @ errs2)
                match xs with
                | [] -> nextAggR
                | nextX::nextXs -> loop nextAggR (idx + 1u) nextX nextXs
            | JsonResult.Error errs, _
            | _, JsonResult.Error errs ->
                let nextAggR = JsonResult.Error errs
                match xs with
                | [] -> nextAggR
                | nextX::nextXs -> loop nextAggR (idx + 1u) nextX nextXs
        let singletonArrayλ = (do ()); fun (x: 'a) -> [|x|]
        D.list >=> function
        | [] -> JsonResult.Ok [||]
        | [x] -> decode x |> JsonResult.map singletonArrayλ
        | x::xs -> loop (Array.zeroCreate (List.length xs + 1) |> JsonResult.Ok) 0u x xs

    let arrayWithAlt2 (decode: JsonReader<'a>) : JsonReader<'a[]> =
        let idλ = (do ()); fun (x: JsonFailure list) -> x
        let singletonArrayλ = (do ()); fun (x: 'a) -> [|x|]
        let rec loop (aggR: Result<'a[],JsonFailure list list>) idx lastX xs =
            let xR = JsonResult.withIndexTag idx decode lastX
            match aggR, xR with
            | Result.Ok agg, JsonResult.Ok x ->
                agg.[int idx] <- x
                match xs with
                | [] -> JsonResult.Ok agg
                | nextX::nextXs -> loop aggR (idx + 1u) nextX nextXs
            | Result.Error errss, JsonResult.Error errs ->
                let nextErrss = errs :: errss
                match xs with
                | [] -> JsonResult.Error (List.rev nextErrss |> List.collect idλ)
                | nextX::nextXs -> loop (Result.Error nextErrss) (idx + 1u) nextX nextXs
            | Result.Error errss, _ ->
                match xs with
                | [] -> JsonResult.Error (List.rev errss |> List.collect idλ)
                | nextX::nextXs -> loop aggR (idx + 1u) nextX nextXs
            | _, JsonResult.Error errs ->
                match xs with
                | [] -> JsonResult.Error errs
                | nextX::nextXs -> loop (Result.Error [errs]) (idx + 1u) nextX nextXs
        D.list >=> function
        | [] -> JsonResult.Ok [||]
        | [x] -> decode x |> JsonResult.map singletonArrayλ
        | x::xs -> loop (Array.zeroCreate (List.length xs + 1) |> Result.Ok) 0u x xs

    let arrayWithAlt3 (decode: JsonReader<'a>) : JsonReader<'a[]> =
        let idλ = (do ()); fun (x: JsonFailure list) -> x
        let singletonArrayλ = (do ()); fun (x: 'a) -> [|x|]
        let rec goodPath (agg: 'a[]) idx lastX xs =
            match JsonResult.withIndexTag idx decode lastX with
            | JsonResult.Ok x ->
                agg.[int idx] <- x
                match xs with
                | [] -> JsonResult.Ok agg
                | nextX::nextXs -> goodPath agg (idx + 1u) nextX nextXs
            | JsonResult.Error errs ->
                match xs with
                | [] -> JsonResult.Error errs
                | nextX::nextXs -> badPath [errs] (idx + 1u) nextX nextXs
        and badPath (aggErrs: JsonFailure list list) idx lastX xs =
            match JsonResult.withIndexTag idx decode lastX with
            | JsonResult.Error errs ->
                let nextErrss = errs::aggErrs
                match xs with
                | [] -> JsonResult.Error (List.rev nextErrss |> List.collect idλ)
                | nextX::nextXs -> badPath nextErrss (idx + 1u) nextX nextXs
            | _ ->
                match xs with
                | [] -> JsonResult.Error (List.rev aggErrs |> List.collect idλ)
                | nextX::nextXs -> badPath aggErrs (idx + 1u) nextX nextXs
        D.list >=> function
        | [] -> JsonResult.Ok [||]
        | [x] -> decode x |> JsonResult.map singletonArrayλ
        | x::xs -> goodPath (Array.zeroCreate (List.length xs + 1)) 0u x xs

    let arrayWithAlt3Quick (decode: JsonReader<'a>) : JsonReader<'a[]> =
        let idλ = (do ()); fun (x: JsonFailure list) -> x
        let singletonArrayλ = (do ()); fun (x: 'a) -> [|x|]
        let rec goodPath (agg: 'a[]) idx lastX xs =
            match JsonResult.withIndexTag idx decode lastX with
            | JsonResult.Ok x ->
                agg.[int idx] <- x
                match xs with
                | [] -> JsonResult.Ok agg
                | nextX::nextXs -> goodPath agg (idx + 1u) nextX nextXs
            | JsonResult.Error errs ->
                JsonResult.Error errs
        D.list >=> function
        | [] -> JsonResult.Ok [||]
        | [x] -> decode x |> JsonResult.map singletonArrayλ
        | x::xs -> goodPath (Array.zeroCreate (List.length xs + 1)) 0u x xs

[<Config(typeof<CoreConfig>)>]
type DecodeList () =
    let testList =
        E.listWith E.number
            [ for i in 1..10000 do
                yield string i ]

    // let array = ListDecoders.arrayWith D.number
    // let arrayAlt = ListDecoders.arrayWithAlt D.number
    let arrayAlt2 = ListDecoders.arrayWithAlt2 D.number
    let arrayAlt3 = ListDecoders.arrayWithAlt3 D.number

    // let list = D.listWith D.number
    // let listAlt = arrayAlt |> JsonReader.map List.ofArray
    // let listAlt2 = arrayAlt2 |> JsonReader.map List.ofArray
    // let listAlt3 = arrayAlt3 |> JsonReader.map List.ofArray

    [<Setup>]
    member x.Setup() =
        let result = arrayAlt3 testList
        match result with
        | JsonResult.Ok _ -> ()
        | _ -> failwith (JsonResult.summarize result)

    // [<Benchmark>]
    // member x.Current_To_Array () =
    //     array testList

    // [<Benchmark>]
    // member x.Current_To_List () =
    //     list testList

    // [<Benchmark>]
    // member x.Alt_To_Array () =
    //     arrayAlt testList

    // [<Benchmark>]
    // member x.Alt_To_List () =
    //     listAlt testList

    [<Benchmark>]
    member x.Alt2_To_Array () =
        arrayAlt2 testList

    // [<Benchmark>]
    // member x.Alt2_To_List () =
    //     listAlt2 testList

    [<Benchmark>]
    member x.Alt3_To_Array () =
        arrayAlt3 testList


[<Config(typeof<CoreConfig>)>]
type DecodeListWithErrors () =
    let testList =
        E.listWith E.string
            [ for i in 1..10000 do
                yield (string i + "λ") ]

    // let array = ListDecoders.arrayWith D.number
    // let arrayAlt = ListDecoders.arrayWithAlt D.number
    let arrayAlt2 = ListDecoders.arrayWithAlt2 D.number
    let arrayAlt3 = ListDecoders.arrayWithAlt3 D.number

    // let list = D.listWith D.number
    // let listAlt = arrayAlt |> JsonReader.map List.ofArray
    // let listAlt2 = arrayAlt2 |> JsonReader.map List.ofArray
    // let listAlt3 = arrayAlt3 |> JsonReader.map List.ofArray

    [<Setup>]
    member x.Setup() =
        let result = arrayAlt3 testList
        match result with
        | JsonResult.Ok _ -> failwith "Should be failures, but were none"
        | _ -> ()

    // [<Benchmark>]
    // member x.Current_To_Array () =
    //     array testList

    // [<Benchmark>]
    // member x.Current_To_List () =
    //     list testList

    // [<Benchmark>]
    // member x.Alt_To_Array () =
    //     arrayAlt testList

    // [<Benchmark>]
    // member x.Alt_To_List () =
    //     listAlt testList

    [<Benchmark>]
    member x.Alt2_To_Array () =
        arrayAlt2 testList

    // [<Benchmark>]
    // member x.Alt2_To_List () =
    //     listAlt2 testList

    [<Benchmark>]
    member x.Alt3_To_Array () =
        arrayAlt3 testList

[<Config(typeof<CoreConfig>)>]
type DecodeJsonObjectToPropertyList () =
    let testObj =
        let rec inner i jObj =
            match i with
            | 0u -> jObj
            | _ -> JsonObject.add (string i) (E.uint32 i) jObj |> inner (i - 1u)
        inner 10000u JsonObject.empty

    let objectReader = JsonObject.toPropertyListWithCustomKey JsonResult.Ok D.json

    [<Benchmark>]
    member x.WithTransferFunction () =
         objectReader testObj

// module Method1 =
//     open Chiron.ObjectReader.Operators

//     type ComplexType =
//         { a: int list option
//           b: ChildType
//           c: MixinType }
//     and ChildType =
//         { d: WrapperType
//           e: byte array }
//     and MixinType =
//         { f: int list }
//     and [<Struct>] WrapperType =
//         | Sad of string

//     module Encoders =
//         module WrapperType =
//             let toJson (Sad x) = Json.ofString x
//             let fromJson = Optic.get Json.Optics.String_ >> Result.map Sad
//         module MixinType =
//             let mk f = { f = f }
//             let encode x = JsonObject.add "f" (Json.ofListWith Json.ofInt32 x.f)
//             let decode = mk <!> JsonObject.read "f"
//         module ChildType =
//             let mk d e = { d = d; e = e }
//             let encode x =
//                 JsonObject.add "d" (WrapperType.toJson x.d)
//              >> JsonObject.add "e" (Json.ofBytes x.e)
//             let decode =
//                 mk
//                 <!> JsonObject.readWith WrapperType.fromJson "d"
//                 <*> JsonObject.read "e"
//         module ComplexType =
//             let mk a b c = { a = a; b = b; c = c }
//             let encode x =
//                 JsonObject.writeOptWith (Json.ofListWith Json.ofInt32) "a" x.a
//              >> JsonObject.writeObjWith (ChildType.encode) "b" x.b
//              >> JsonObject.mixinObj (MixinType.encode) x.c
//             let decode =
//                 mk
//                 <!> JsonObject.readOpt "a"
//                 <*> (JsonObject.tryGetOrFail "b" >> Result.bind (Optic.get Json.Optics.Object_) >> Result.bind ChildType.decode)
//                 <*> MixinType.decode

//     type ComplexType with
//         static member Encode x = Encoders.ComplexType.encode x
//         static member Decode (_:ComplexType) = Encoders.ComplexType.decode

//     type ChildType with
//         static member Encode x = Encoders.ChildType.encode x
//         static member Decode (_:ChildType) = Encoders.ChildType.decode

//     type MixinType with
//         static member Encode x = Encoders.MixinType.encode x
//         static member Decode (_:MixinType) = Encoders.MixinType.decode

//     type WrapperType with
//         static member ToJson x = Encoders.WrapperType.toJson x
//         static member FromJson (_:WrapperType) = Encoders.WrapperType.fromJson

//     module Constants =
//         let expected =
//             { a = Some [ 2; 4; 6; 8 ]
//               b = { d = Sad "winter"
//                     e = "Hello world!" |> System.Text.Encoding.UTF8.GetBytes }
//               c = { f = [ 1; 2; 3; 4 ] } }

// module Method2 =
//     open Chiron.ObjectReader.Operators

//     type ComplexType =
//         { a: int list option
//           b: ChildType
//           c: MixinType }
//     and ChildType =
//         { d: WrapperType
//           e: byte array }
//     and MixinType =
//         { f: int list }
//     and [<Struct>] WrapperType =
//         | Sad of string

//     type WrapperType with
//         static member ToJson (Sad x) = Json.ofString x
//         static member FromJson (_:WrapperType) = Optic.get Json.Optics.String_ >> Result.map Sad

//     type MixinType with
//         static member Encode x = JsonObject.add "f" (Json.ofListWith Json.ofInt32 x.f)
//         static member Decode (_:MixinType) = (fun f -> { f = f }) <!> JsonObject.read "f"

//     type ChildType with
//         static member Encode x =
//             JsonObject.add "d" (WrapperType.ToJson x.d)
//             >> JsonObject.add "e" (Json.ofBytes x.e)
//         static member Decode (_:ChildType) =
//             (fun d e -> { d = d; e = e })
//             <!> JsonObject.readWith (WrapperType.FromJson Unchecked.defaultof<_>) "d"
//             <*> JsonObject.read "e"

//     type ComplexType with
//         static member Encode x =
//             JsonObject.writeOptWith (Json.ofListWith Json.ofInt32) "a" x.a
//             >> JsonObject.writeObjWith (ChildType.Encode) "b" x.b
//             >> JsonObject.mixinObj (MixinType.Encode) x.c
//         static member Decode (_:ComplexType) =
//             (fun a b c -> { a = a; b = b; c = c })
//             <!> JsonObject.readOpt "a"
//             <*> (JsonObject.tryGetOrFail "b" >> Result.bind (Optic.get Json.Optics.Object_) >> Result.bind (ChildType.Decode Unchecked.defaultof<_>))
//             <*> (MixinType.Decode Unchecked.defaultof<_>)

//     module Constants =
//         let expected =
//             { a = Some [ 2; 4; 6; 8 ]
//               b = { d = Sad "winter"
//                     e = "Hello world!" |> System.Text.Encoding.UTF8.GetBytes }
//               c = { f = [ 1; 2; 3; 4 ] } }

// module Method3 =
//     open Chiron.ObjectReader.Operators

//     let readObjWith (reader : ObjectReader<'a>) k jsonObject : JsonResult<'a> =
//         JsonObject.tryGetOrFail k jsonObject
//         |> Result.bind (Optic.get Json.Optics.Object_)
//         |> Result.bind reader

//     let inline readObjInfer k jsonObject =
//         let decoder = (^a : (static member Decode : ObjectReader<'a>) ())
//         readObjWith decoder k jsonObject

//     let writeObjWith (writer: ObjectWriter<'a>) (k: string) (v: 'a) (jsonObject: JsonObject) : JsonObject =
//         let json = writer v JsonObject.empty |> JsonObject.toJson
//         JsonObject.add k json jsonObject

//     let inline writeObjWithDefault (defaults : ^def) (k : string) (v : ^a) (jsonObject : JsonObject) =
//         let json = ((^a or ^def) : (static member Encode : ^a -> JsonObject -> JsonObject) (v, jsonObject)) |> JsonObject.toJson
//         JsonObject.add k json jsonObject

//     let inline writeObj (k : string) (v : ^a) (jsonObject : JsonObject) =
//         writeObjWithDefault ChironDefaults k v jsonObject

//     let inline mixinObjInfer v jObj =
//         (^a : (static member Encode : 'a -> JsonObject -> JsonObject) (v, jObj))

//     type ComplexType =
//         { a: int list option
//           b: ChildType
//           c: MixinType }
//     and ChildType =
//         { d: WrapperType
//           e: byte array }
//     and MixinType =
//         { f: int list }
//     and [<Struct>] WrapperType =
//         | Sad of string

//     module WrapperType =
//         let toJson (Sad x) = Json.ofString x
//         let fromJson = Optic.get Json.Optics.String_ >> Result.map Sad
//     type WrapperType with
//         static member ToJson x = WrapperType.toJson x
//         static member FromJson (_:WrapperType) = WrapperType.fromJson

//     module MixinType =
//         let mk f = { f = f }
//         let encode x jobj = jobj |> JsonObject.write "f" x.f
//         let decode = mk <!> JsonObject.read "f"
//     type MixinType with
//         static member Encode (x, jobj) = MixinType.encode x jobj
//         static member Decode = MixinType.decode

//     module ChildType =
//         let mk d e = { d = d; e = e }
//         let encode x jobj =
//             jobj
//             |> JsonObject.write "d" x.d
//             |> JsonObject.write "e" x.e
//         let decode =
//             mk
//             <!> JsonObject.read "d"
//             <*> JsonObject.read "e"
//     type ChildType with
//         static member Encode (x, jobj) = ChildType.encode x jobj
//         static member Decode = ChildType.decode

//     module ComplexType =
//         let mk a b c = { a = a; b = b; c = c }
//         let encode x jobj =
//             jobj
//             |> JsonObject.writeOpt "a" x.a
//             |> writeObj "b" x.b
//             |> mixinObjInfer x.c
//         let decode =
//             mk
//             <!> JsonObject.readOpt "a"
//             <*> readObjInfer "b"
//             <*> MixinType.decode
//     type ComplexType with
//         static member Encode (x, jobj) = ComplexType.encode x jobj
//         static member Decode = ComplexType.decode

//     module Constants =
//         let expected =
//             { a = Some [ 2; 4; 6; 8 ]
//               b = { d = Sad "winter"
//                     e = "Hello world!" |> System.Text.Encoding.UTF8.GetBytes }
//               c = { f = [ 1; 2; 3; 4 ] } }

// module Constants =
//     let thing = """{"f":[1,2,3,4],"a":[2,4,6,8],"b":{"e":"SGVsbG8gd29ybGQh","d":"winter"}}"""
//     let thing2 = """{"a":[2,4,6,8],"b":{"d":"winter","e":"SGVsbG8gd29ybGQh"},"f":[1,2,3,4]}"""

// [<Config(typeof<CoreConfig>)>]
// type Decode () =
//     let jObj = Json.parseOrThrow Constants.thing |> Optic.get Json.Optics.Object_ |> function | Ok x -> x; | Error _ -> failwithf "Nope"

//     [<Benchmark>]
//     member __.Prebuild () : JsonResult<Method1.ComplexType> =
//         Method1.ComplexType.Decode Unchecked.defaultof<_> jObj

//     [<Benchmark>]
//     member __.PrebuildAlt () : JsonResult<Method3.ComplexType> =
//         Method3.ComplexType.Decode jObj

//     [<Benchmark(Baseline=true)>]
//     member __.NoPrebuild () : JsonResult<Method2.ComplexType> =
//         Method2.ComplexType.Decode Unchecked.defaultof<_> jObj

// [<Config(typeof<CoreConfig>)>]
// type Encode () =
//     [<Benchmark>]
//     member __.Prebuild () : Json =
//         JsonObject.build Method1.ComplexType.Encode Method1.Constants.expected

//     [<Benchmark>]
//     member __.PrebuildAlt () : Json =
//         Method3.ComplexType.Encode (Method3.Constants.expected, JsonObject.empty) |> JsonObject.toJson

//     [<Benchmark(Baseline=true)>]
//     member __.NoPrebuild () : Json =
//         JsonObject.build Method2.ComplexType.Encode Method2.Constants.expected
