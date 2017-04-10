namespace Chiron.Tests.Functional

open System
open Chiron
open Chiron.Operators
open Xunit
open Swensen.Unquote

module D = Json.Decode
module E = Json.Encode

[<AutoOpen>]
module Constants =
    let t1 =
        E.propertyList
            [ "bool", E.bool true
              "number", E.decimal 2M ]

    let t2 =
        E.propertyList
            [ "bool", E.bool false
              "number", E.decimal 2M ]

    type Testing =
        { one: int option
          two: bool
          three: int }

    module Testing =
        let mk o t r = { one = o; two = t; three = r }
        let encode x jObj =
            jObj
            |> E.required (E.optionWith E.int) "1" x.one
            |> E.required E.bool "2" x.two
            |> E.required E.int "3" x.three
        let decode =
            mk
            <!> D.required (D.optionWith D.int) "1"
            <*> D.required D.bool "2"
            <*> D.required D.int "3"

    type Testing with
        static member Encode (x: Testing) =
            Testing.encode x
        static member Decode (jObj) =
            Testing.decode jObj

    let testJsonObject = E.propertyList [ "1", Json.Null; "2", E.bool true; "3", E.int 42 ]
    let testObject = { one = None; two = true; three = 42 }
    let [<Literal>] testJson1 = """{"1":null,"2":true,"3":42}"""

(* Functional

   Tests to exercise the basic functional components of the Json<'a>
   type and combinators thereof. *)
module JsonTransformer =
    open JsonTransformer

    [<Fact>]
    let ``Json.init returns correct values`` () =
        Json.init 1 t1 =! (JPass 1,  t1)

    [<Fact>]
    let ``Json.error returns correct values`` () =
        Json.error (SingleFailure NoInput) t1 =! (JsonResult.noInput, t1)

    [<Fact>]
    let ``Json.bind returns correct values`` () =
        Json.bind (fun x -> Json.init (x * 3)) (Json.init 2) t1 =! (JPass 6, t1)
        Json.bind (fun x -> Json.init (x * 3)) (Json.error (SingleFailure NoInput)) t1 =! ((JsonResult.noInput : JsonResult<int>), t1)

    [<Fact>]
    let ``Json.apply returns correct values`` () =
        Json.apply (Json.init 2) (Json.init (fun x -> x * 3)) t1 =! (JPass 6, t1)
        Json.apply (Json.error (SingleFailure NoInput)) (Json.init (fun x -> x * 3)) t1 =! (JsonResult.noInput, t1)

    [<Fact>]
    let ``Json.map returns correct values`` () =
        Json.map (fun x -> x * 3) (Json.init 2) t1 =! (JPass 6, t1)
        Json.map (fun x -> x * 3) (Json.error (SingleFailure NoInput)) t1 =! (JsonResult.noInput, t1)

    [<Fact>]
    let ``Json.map2 returns correct values`` () =
        Json.map2 (*) (Json.init 2) (Json.init 3) t1 =! (JPass 6, t1)
        Json.map2 (*) (Json.error (SingleFailure NoInput)) (Json.init 3) t1 =! (JsonResult.noInput, t1)
        Json.map2 (*) (Json.init 2) (Json.error (SingleFailure NoInput)) t1 =! (JsonResult.noInput, t1)
        Json.map2 (*) (Json.error (SingleFailure (PropertyNotFound "s"))) (Json.error (SingleFailure NoInput)) t1 =! (JFail (SingleFailure (PropertyNotFound "s")), t1)

(* Lens

   Tests to exercise the functional lens based access to Json
   data structures. *)
module Lenses =
    let private id_ =
        JPass, (fun a _ -> a)

    let private prism_ =
        Optics.compose (Optics.Json.Property_ "bool") Optics.Json.Bool_

    [<Fact>]
    let ``Optics.get returns correct values`` () =
        Optics.get id_ t1 =! JPass t1

    [<Fact>]
    let ``Optics.get with Lens returns correct values`` () =
        Optics.get prism_ t1 =! JPass true

    [<Fact>]
    let ``Optics.set with Lens returns correct values`` () =
        Optics.set id_ (E.bool false) t1 =! E.bool false

    [<Fact>]
    let ``Optics.set with Prism returns correct values`` () =
        let json =
            Optics.set prism_ false t1
            |> function
            | Object o -> E.jsonObject o
            | json -> json
        json =! t2

(* Parsing *)

module Parsing =
    [<Fact>]
    let ``Json.parse returns correct values`` () =
        Json.parse "\"hello\"" =! JPass (E.string "hello")
        Json.parse "\"\"" =! JPass (E.string "")
        Json.parse "\"\\n\"" =! JPass (E.string "\n")
        Json.parse "\"\\u005c\"" =! JPass (E.string "\\")
        Json.parse "\"푟\"" =! JPass (E.string "푟")

        Json.parse null =! JsonResult.noInput

module Formatting =
    [<Fact>]
    let ``Json.format returns correct values`` () =
        Json.format t1 =! """{"bool":true,"number":2}"""

        Json.format (E.string "hello") =! "\"hello\""
        Json.format (E.string "he\nllo") =! "\"he\\nllo\""
        Json.format (E.string "") =! "\"\""
        Json.format (E.string "푟") =! "\"푟\""
        Json.format (E.string "\t") =! "\"\\t\""

    // [<Fact>]
    // let ``escape is not pathological for cases with escapes`` () =
    //     let testObj =
    //         Json.MakeArray
    //             [ for i in 1..100000 do
    //                 yield Json.infer <| "he\nllo\u0006\t 푟 " + string i ]
    //     let testString = Json.format testObj
    //     let sw = System.Diagnostics.Stopwatch.StartNew()
    //     let escaped = Escaping.escape testString
    //     let time = sw.ElapsedMilliseconds
    //     printfn "String length: %i, JSON escaped length: %i, Time: %i ms" (String.length testString) (String.length escaped) time
    //     Assert.InRange(time, 0L, 5000L)

    // [<Fact>]
    // let ``escape is not pathological for non-escaped cases`` () =
    //     let testString = String.replicate 2500000 "a"
    //     let sw = System.Diagnostics.Stopwatch.StartNew()
    //     let escaped = Escaping.escape testString
    //     let time = sw.ElapsedMilliseconds
    //     printfn "String length: %i, JSON escaped length: %i, Time: %i ms" (String.length testString) (String.length escaped) time
    //     Assert.InRange(time, 0L, 5000L)

(* Mapping

   Tests exercising mapping functions between Json and other F#
   data structures. *)

module Serialization =
    [<Fact>]
    let ``unit round-trips for example value`` () =
        D.unit (E.unit ()) =! JPass ()

    [<Fact>]
    let ``true round-trips for example value`` () =
        D.bool (E.bool true) =! JPass true

    [<Fact>]
    let ``false round-trips for example value`` () =
        D.bool (E.bool false) =! JPass false

    let fortytwo = E.int 42

    [<Fact>]
    let ``decimal round-trips for example value`` () =
        D.decimal (E.decimal 42M) =! JPass 42M

    [<Fact>]
    let ``float round-trips for example value`` () =
        D.float (E.float 42.0) =! JPass 42.0

    [<Fact>]
    let ``int round-trips for example value`` () =
        D.int (E.int 42) =! JPass 42

    [<Fact>]
    let ``int16 round-trips for example value`` () =
        D.int16 (E.int16 42s) =! JPass 42s

    [<Fact>]
    let ``int64 round-trips for example value`` () =
        D.int64 (E.int64 42L) =! JPass 42L

    [<Fact>]
    let ``single round-trips for example value`` () =
        D.single (E.single 42.0f) =! JPass 42.0f

    [<Fact>]
    let ``uint16 round-trips for example value`` () =
        D.uint16 (E.uint16 42us) =! JPass 42us

    [<Fact>]
    let ``uint32 round-trips for example value`` () =
        D.uint32 (E.uint32 42u) =! JPass 42u

    [<Fact>]
    let ``uint64 round-trips for example value`` () =
        D.uint64 (E.uint64 42UL) =! JPass 42UL

    [<Fact>]
    let ``uint64 on bad value returns expected value`` () =
        let result = D.uint64 (E.number "-2")
        match result with
        | JFail (SingleFailure (DeserializationError (t, err))) when t = typeof<uint64> && err.Message = "Value was either too large or too small for a UInt64." -> ()
        | JFail f -> failwithf "Did not match expected error: %s" (JsonFailure.summarize f)
        | _ -> failwithf "Unexpectedly succeeded"

    [<Fact>]
    let ``string round-trips for example value`` () =
        D.string (E.string "hello") =! JPass "hello"

    [<Fact>]
    let ``dateTime round-trips for example value`` () =
        let testValue = DateTime (2015, 2, 20, 14, 36, 21, DateTimeKind.Utc)
        D.dateTime (E.dateTime testValue) =! JPass testValue

    [<Fact>]
    let ``dateTimeOffset round-trips for example value`` () =
        let testValue = DateTimeOffset (2015, 4, 15, 13, 45, 55, TimeSpan.Zero)
        D.dateTimeOffset (E.dateTimeOffset testValue) =! JPass testValue

    [<Fact>]
    let ``guid round-trips for example value`` () =
        let guid = Guid.NewGuid()
        D.guid (E.guid guid) =! JPass guid

    [<Fact>]
    let ``bytes round-trips for example value`` () =
        let bytes = "Hello Test!"B
        D.bytes (E.bytes bytes) =! JPass bytes

module Special =
    [<Fact>]
    let ``Parsing reverses order of object elements`` () =
        test <@ testJson1 |> Json.parse = JPass testJsonObject @>

    [<Fact>]
    let ``Decoding preserves order of object elements`` () =
        test <@ testJson1 |> Json.parse |> JsonResult.bind (D.jsonObjectWith Testing.decode) = JPass testObject @>

    [<Fact>]
    let ``Formatting preserves order of object elements`` () =
        test <@ testJsonObject |> Json.format = testJson1 @>

    [<Fact>]
    let ``Encoding preserves order of object elements`` () =
        test <@ testObject |> E.jsonObjectWith Testing.encode |> Json.format = testJson1 @>

    [<Fact>]
    let ``Parsing preserves order of array elements`` () =
        test <@ """["1","2","3"]""" |> Json.parse = JPass (E.list [ E.string "1"; E.string "2"; E.string "3" ]) @>

    [<Fact>]
    let ``Decoding preserves order of array elements`` () =
        test <@ """["1","2","3"]""" |> Json.parse |> JsonResult.bind (D.listWith D.string) = JPass ["1"; "2"; "3"] @>

    [<Fact>]
    let ``Formatting preserves order of array elements`` () =
        test <@ E.list [ E.string "1"; E.string "2"; E.string "3" ] |> Json.format = """["1","2","3"]""" @>

    [<Fact>]
    let ``Encoding preserves order of array elements`` () =
        test <@ ["1";"2";"3"] |> E.listWith E.string |> Json.format = """["1","2","3"]""" @>

module Inference =
    open Chiron.Inference

    [<Fact>]
    let ``Inferred round-trip on true returns expected value`` () =
        Json.decode (Json.encode true) =! JPass true

    [<Fact>]
    let ``Inferred round-trip on false returns expected value`` () =
        Json.decode (Json.encode false) =! JPass false

    let fortytwo = Json.encode 42

    [<Fact>]
    let ``Inferred round-trip to decimal returns expected value`` () =
        Json.decode fortytwo =! JPass 42M

    [<Fact>]
    let ``Inferred round-trip to float returns expected value`` () =
        Json.decode fortytwo =! JPass 42.0

    [<Fact>]
    let ``Inferred round-trip to int returns expected value`` () =
        Json.decode fortytwo =! JPass 42

    [<Fact>]
    let ``Inferred round-trip to int16 returns expected value`` () =
        Json.decode fortytwo =! JPass 42s

    [<Fact>]
    let ``Inferred round-trip to int64 returns expected value`` () =
        Json.decode fortytwo =! JPass 42L

    [<Fact>]
    let ``Inferred round-trip to single returns expected value`` () =
        Json.decode fortytwo =! JPass 42.0f

    [<Fact>]
    let ``Inferred round-trip to uint16 returns expected value`` () =
        Json.decode fortytwo =! JPass 42us

    [<Fact>]
    let ``Inferred round-trip to uint32 returns expected value`` () =
        Json.decode fortytwo =! JPass 42u

    [<Fact>]
    let ``Inferred round-trip to uint64 returns expected value`` () =
        Json.decode fortytwo =! JPass 42UL

    [<Fact>]
    let ``Inferred round-trip to uint64 on bad value returns expected value`` () =
        let result : JsonResult<uint64> = Json.decode (E.number "-2")
        match result with
        | JFail (SingleFailure (DeserializationError (t, err))) when t = typeof<uint64> && err.Message = "Value was either too large or too small for a UInt64." -> ()
        | JFail f -> failwithf "Did not match expected error: %s" (JsonFailure.summarize f)
        | _ -> failwithf "Unexpectedly succeeded"

    [<Fact>]
    let ``Inferred round-trip to string returns expected value`` () =
        Json.decode (Json.encode "hello") =! JPass "hello"

    [<Fact>]
    let ``Inferred round-trip to datetime returns expected value`` () =
        Json.decode (Json.encode "Fri, 20 Feb 2015 14:36:21 GMT") =! JPass (DateTime (2015, 2, 20, 14, 36, 21, DateTimeKind.Utc))

    [<Fact>]
    let ``Inferred round-trip to datetimeoffset returns expected value`` () =
        Json.decode (Json.encode "2015-04-15T13:45:55Z") =! JPass (DateTimeOffset (2015, 4, 15, 13, 45, 55, TimeSpan.Zero))

    [<Fact>]
    let ``Inferred round-trip to guid returns expected value`` () =
        Json.decode (Json.encode "0123456789ABCDEFFEDCBA9876543210") =! JPass (Guid "0123456789ABCDEFFEDCBA9876543210")

    [<Fact>]
    let ``Inferred round-trip on array returns correct values`` () =
        Json.decode (Json.encode [ "hello"; "world" ]) =! JPass [| "hello"; "world" |]

    [<Fact>]
    let ``Inferred round-trip on list returns correct values`` () =
        Json.decode (Json.encode [ "hello"; "world" ]) =! JPass [ "hello"; "world" ]

    [<Fact>]
    let ``Inferred round-trip on None returns correct values`` () =
        let none : string option = None
        Json.decode (Json.encode none) =! JPass none

    [<Fact>]
    let ``Inferred round-trip on None with different types returns correct values`` () =
        Json.decode (Json.encode (None: int option)) =! JPass (None: string option)

    [<Fact>]
    let ``Inferred round-trip on map returns correct values`` () =
        Json.decode (E.propertyList [ "one", E.decimal 1M; "two", E.decimal 2M ]) =! JPass (Map.ofList [ "one", 1; "two", 2 ])

    [<Fact>]
    let ``Inferred round-trip on set returns correct values`` () =
        Json.decode (Json.encode [ "one"; "two" ]) =! JPass (set [ "one"; "two" ])

    [<Fact>]
    let ``Inferred round-trip on Some returns correct values`` () =
        Json.decode (Json.encode (Some "hello")) =! JPass (Some "hello")

    [<Fact>]
    let ``Inferred round-trip on Some returns correct values (no wrapper)`` () =
        Json.decode (Json.encode "hello") =! JPass (Some "hello")

    [<Fact>]
    let ``Inferred round-trip on 2-Tuple returns correct values`` () =
        Json.decode (Json.encode [ Json.encode "hello"; Json.encode 42M ]) =! JPass ("hello", 42)

    [<Fact>]
    let ``Inferred round-trip on 3-Tuple returns correct values`` () =
        Json.decode (Json.encode [ Json.encode "hello"; Json.encode 42M; Json.encode true ]) =! JPass ("hello", 42, true)

    [<Fact>]
    let ``Inferred round-trip on 4-Tuple returns correct values`` () =
        Json.decode (Json.encode [ Json.encode "hello"; Json.encode 42M; Json.encode true; Json.encode (None: uint16 option) ])
            =! JPass ("hello", 42, true, (None: string option))

    [<Fact>]
    let ``Inferred round-trip on 5-Tuple returns correct values`` () =
        Json.decode (Json.encode [ Json.encode "hello"; Json.encode 42M; Json.encode true; Json.encode (None: uint16 option); Json.encode -4 ])
            =! JPass ("hello", 42, true, (None: string option), -4L)

    [<Fact>]
    let ``Inferred round-trip on 6-Tuple returns correct values`` () =
        Json.decode (Json.encode [ Json.encode "hello"; Json.encode 42M; Json.encode true; Json.encode (None: uint16 option); Json.encode -4; Json.encode "Test" ])
            =! JPass ("hello", 42, true, (None: string option), -4L, "Test")

    [<Fact>]
    let ``Inferred round-trip on 7-Tuple returns correct values`` () =
        Json.decode (Json.encode [ Json.encode "hello"; Json.encode 42M; Json.encode true; Json.encode (None: uint16 option); Json.encode -4; Json.encode "Test"; Json.encode -0.0 ])
            =! JPass ("hello", 42, true, (None: string option), -4L, "Test", 0M)

module WithTestRecord =
    open Inference
    module DI = Inference.Json.Decode
    module EI = Inference.Json.Encode

    type Test =
        { String: string
          Number : int option
          Values: bool list
          Json: Json }

    module rec Json =
        module Mixin =
            let test =
                let boolList = E.listWith E.bool
                fun x jObj ->
                    jObj
                    |> EI.required "string" x.String
                    |> EI.optional "number" x.Number
                    |> E.required boolList "values" x.Values
                    |> E.required E.json "json" x.Json
        module Encode =
            let test = E.buildWith Mixin.test
        module Decode =
            let test =
                let inner =
                    (fun s n v j -> { String = s; Number = n; Values = v; Json = j })
                    <!> DI.required "string"
                    <*> D.optional D.int "number"
                    <*> D.required (D.listWith D.bool) "values"
                    <*> D.required D.json "json"
                D.jsonObject >=> inner

    type Test with
        static member Mixin (x: Test, jObj: JsonObject) =
            Json.Mixin.test x jObj
        static member ToJson (x: Test) =
            Json.Encode.test x
        static member FromJson (_: Test) =
            Json.Decode.test

    let testJson =
        E.propertyList
            [ "string", E.string "hello"
              "number", E.decimal 42M
              "values", E.listWith E.bool [ true; false ]
              "json", E.propertyList [ "hello", E.string "world" ] ]

    let testInstance =
        { String = "hello"
          Number = Some 42
          Values = [ true; false ]
          Json = E.propertyList [ "hello", E.string "world" ] }

    [<Fact>]
    let ``Json.decode with custom typed returns correct values`` () =
        Json.decode testJson =! JPass testInstance

    let testJsonWithNullOption =
        E.propertyList
            [ "string", E.string "hello"
              "number", Json.Null
              "values", E.listWith E.bool []
              "json", E.propertyList [ "hello", E.string "world" ] ]

    let testInstanceWithNoneOption =
        { String = "hello"
          Number = None
          Values = [ ]
          Json = E.propertyList [ "hello", E.string "world" ] }

    [<Fact(Skip="To be considered")>]
    let ``Json.decode with null option value`` () =
        Json.decode testJsonWithNullOption =! JPass testInstanceWithNoneOption

    let testJsonWithMissingOption =
        E.propertyList
            [ "string", E.string "hello"
              "values", E.listWith E.bool []
              "json", E.propertyList [ "hello", Json.encode "world" ] ]

    [<Fact>]
    let ``Json.decode with missing value`` () =
        Json.decode testJsonWithMissingOption =! JPass testInstanceWithNoneOption

    [<Fact>]
    let ``Json.encode with default value`` () =
        Json.encode testInstanceWithNoneOption =! testJsonWithMissingOption

    let testJsonWithNoValues =
        E.propertyList
            [ "string", E.string "hello"
              "number", E.decimal 42M
              "json", E.propertyList [ "hello", E.string "world" ] ]

    [<Fact>]
    let ``Json.decode with invalid value includes missing member name`` () =
        let x : JsonResult<Test> = Json.decode testJsonWithNoValues
        x =! JFail (SingleFailure (PropertyNotFound "values"))

    [<Fact>]
    let ``Json.encode with custom types returns correct values`` () =
        Json.encode testInstance =! testJson

module WithTestUnion =
    open Operators
    module EI = Inference.Json.Encode
    module DI = Inference.Json.Decode

    type TestUnion =
        | One of string
        | Two of int * bool

    module TestUnion =
        let mkTwo i b = Two (i, b)
        let toJsonOne s =
            E.string s
        let encodeTwo (i, b) jObj =
            EI.required "two" i jObj
            |> EI.required "twoble" b
        let fromJsonOne json =
            D.string json
            |> JsonResult.map One
        let decodeTwo =
            mkTwo
            <!> DI.required "two"
            <*> DI.required "twoble"
        let toJson = function
            | One s -> toJsonOne s
            | Two (i, b) -> E.buildWith encodeTwo (i, b)
        let fromJson =
            D.oneOf
                [ fromJsonOne
                  D.jsonObject >=> decodeTwo ]

    type TestUnion with
        static member ToJson (x: TestUnion) =
            TestUnion.toJson x
        static member FromJson (_ : TestUnion, json: Json) =
            TestUnion.fromJson json

// let testUnion =
//     Two (42, true)

// let testUnionJson =
//     Json.infer
//         [ "two", Json.infer [ Json.infer 42M; Json.infer true ] ]

// [<Fact>]
// let ``Json.serialize with union types remains tractable`` () =
//     Json.serialize testUnion =! testUnionJson

// [<Fact>]
// let ``Json.decode with union types remains tractable`` () =
//     Json.decode testUnionJson =! JPass testUnion

// [<Fact>]
// let ``Json.format escapes object keys correctly`` () =
//     let data = Map [ "\u001f", "abc" ]
//     let serialized = Json.serialize data
//     let formatted = Json.format serialized

//     formatted =! """{"\u001F":"abc"}"""

// module Json =
//     let ofDayOfWeek (e : DayOfWeek) =
//         e.ToString "G"
//         |> Json.infer
//     let toDayOfWeek : JsonReader<DayOfWeek> =
//         Chiron.Optic.get Json.Optics.String_
//         >> Result.bind (JsonResult.withThrowingParser (fun s -> Enum.Parse(typeof<DayOfWeek>, s) :?> DayOfWeek))

// type MyDayOfWeekObject =
//     { Bool : bool
//       Day : DayOfWeek }
//     static member ToJson (x:MyDayOfWeekObject) =
//         JsonObject.empty
//         |> JsonObject.write "bool" x.Bool
//         |> JsonObject.writeWith Json.ofDayOfWeek "day_of_week" x.Day
//         |> JsonObject.toJson
//     static member FromJson (_:MyDayOfWeekObject) =
//         D.jsonObject >=>
//         (fun b d -> { Bool = b; Day = d }
//         <!> JsonObject.read "bool"
//         <*> JsonObject.readWith Json.toDayOfWeek "day_of_week")

// let deserializedMonday = { Bool = true; Day = DayOfWeek.Monday }
// let serializedMonday = Json.infer ["bool", Json.infer true; "day_of_week", Json.infer "Monday"]

// [<Fact>]
// let ``Json.readWith allows using a custom deserialization function`` () =
//     Json.decode serializedMonday =! JPass deserializedMonday

// [<Fact>]
// let ``Json.writeWith allows using a custom serialization function`` () =
//     Json.serialize deserializedMonday =! serializedMonday


// // let testWriter x = jsonWriter {
// //     do! JsonObject.write "bool" x.Bool
// //     do! JsonObject.writeWith Json.ofDayOfWeek "day_of_week" x.Day
// // }
// // [<Fact>]
// // let ``Json.writeWith allows using a custom serialization function`` () =
// //     testWriter deserializedMonday =! serializedMonday

// [<Fact>]
// let ``Encode works?`` () =
//     let encode = Json.encode
//     test <@ testJsonObject = encode testObject @>

module WithComplexType =
    type ComplexType =
        { a: int list option
          b: ChildType
          c: MixinType }
    and ChildType =
        { d: WrapperType
          e: byte array }
    and MixinType =
        { f: int list }
    and [<Struct>] WrapperType =
        | Sad of string

    // module Encoders =
    //     module WrapperType =
    //         let toJson (Sad x) = Json.ofString x
    //         let fromJson = Optic.get Json.Optics.String_ >> Result.map Sad
    //     module MixinType =
    //         let mk f = { f = f }
    //         let encode x = JsonObject.add "f" (Json.ofListWith Json.ofInt32 x.f)
    //         let decode = mk <!> JsonObject.read "f"
    //     module ChildType =
    //         let mk d e = { d = d; e = e }
    //         let encode x =
    //             JsonObject.add "d" (WrapperType.toJson x.d)
    //          >> JsonObject.add "e" (Json.ofBytes x.e)
    //         let decode =
    //             mk
    //             <!> JsonObject.readWith WrapperType.fromJson "d"
    //             <*> JsonObject.read "e"
    //     module ComplexType =
    //         let mk a b c = { a = a; b = b; c = c }
    //         let encode x =
    //             JsonObject.writeOptWith (Json.ofListWith Json.ofInt32) "a" x.a
    //          >> JsonObject.writeObjWith (ChildType.encode) "b" x.b
    //          >> JsonObject.mixinObj (MixinType.encode) x.c
    //         let decode =
    //             mk
    //             <!> JsonObject.readOpt "a"
    //             <*> (JsonObject.tryGetOrFail "b" >> Result.bind (Optic.get Json.Optics.Object_) >> Result.bind ChildType.decode)
    //             <*> MixinType.decode

    // type ComplexType with
    //     static member Encode x = Encoders.ComplexType.encode x
    //     static member Decode (_:ComplexType) = Encoders.ComplexType.decode

    // type ChildType with
    //     static member Encode x = Encoders.ChildType.encode x
    //     static member Decode (_:ChildType) = Encoders.ChildType.decode

    // type MixinType with
    //     static member Encode x = Encoders.MixinType.encode x
    //     static member Decode (_:MixinType) = Encoders.MixinType.decode

    // type WrapperType with
    //     static member ToJson x = Encoders.WrapperType.toJson x
    //     static member FromJson (_:WrapperType) = Encoders.WrapperType.fromJson

    // let thing = """{"f":[1,2,3,4],"a":[2,4,6,8],"b":{"e":"SGVsbG8gd29ybGQh","d":"winter"}}"""
    // let thing2 = """{"a":[2,4,6,8],"b":{"d":"winter","e":"SGVsbG8gd29ybGQh"},"f":[1,2,3,4]}"""
    // let expected =
    //     { a = Some [ 2; 4; 6; 8 ]
    //       b = { d = Sad "winter"
    //             e = "Hello world!" |> System.Text.Encoding.UTF8.GetBytes }
    //       c = { f = [ 1; 2; 3; 4 ] } }

    // [<Fact>]
    // let ``complexx`` () =
    //     test <@ JPass expected = (Json.parseOrThrow thing |> Optic.get Json.Optics.Object_ |> Result.bind Encoders.ComplexType.decode) @>
    // [<Fact>]
    // let ``complexy`` () =
    //     test <@ thing2 = (JsonObject.build Encoders.ComplexType.encode expected |> Json.format) @>
