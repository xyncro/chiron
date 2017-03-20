module Chiron.Tests.Functional

open System
open Chiron
open Chiron.Json.Operators
open Chiron.Parsing
open Chiron.Formatting
open Chiron.Serialization
open Xunit
open Swensen.Unquote

(* Cases *)

let private t1 =
    Json.infer
        [ "bool", Json.infer true
          "number", Json.infer 2M ]

let private t2 =
    Json.infer
        [ "bool", Json.infer false
          "number", Json.infer 2M ]

(* Functional

   Tests to exercise the basic functional components of the Json<'a>
   type and combinators thereof. *)

[<Fact>]
let ``Json.init returns correct values`` () =
    Json.init 1 t1 =! (Ok 1,  t1)

[<Fact>]
let ``Json.error returns correct values`` () =
    Json.error NoInput t1 =! (Error [NoInput], t1)

[<Fact>]
let ``Json.bind returns correct values`` () =
    Json.bind (fun x -> Json.init (x * 3)) (Json.init 2) t1 =! (Ok 6, t1)
    Json.bind (fun x -> Json.init (x * 3)) (Json.error NoInput) t1 =! (Error [NoInput], t1)

[<Fact>]
let ``Json.apply returns correct values`` () =
    Json.apply (Json.init 2) (Json.init (fun x -> x * 3)) t1 =! (Ok 6, t1)
    Json.apply (Json.error NoInput) (Json.init (fun x -> x * 3)) t1 =! (Error [NoInput], t1)

[<Fact>]
let ``Json.map returns correct values`` () =
    Json.map (fun x -> x * 3) (Json.init 2) t1 =! (Ok 6, t1)
    Json.map (fun x -> x * 3) (Json.error NoInput) t1 =! (Error [NoInput], t1)

[<Fact>]
let ``Json.map2 returns correct values`` () =
    Json.map2 (*) (Json.init 2) (Json.init 3) t1 =! (Ok 6, t1)
    Json.map2 (*) (Json.error NoInput) (Json.init 3) t1 =! (Error [NoInput], t1)
    Json.map2 (*) (Json.init 2) (Json.error NoInput) t1 =! (Error [NoInput], t1)
    Json.map2 (*) (Json.error PropertyNotFound) (Json.error NoInput) t1 =! (Error [PropertyNotFound], t1)

(* Lens

   Tests to exercise the functional lens based access to Json
   data structures. *)

let private id_ =
    Ok, (fun a _ -> a)

let private prism_ =
    Optic.compose (Optic.compose Json.Optics.Object_ (JsonObject.Optics.key_ "bool")) Json.Optics.Bool_

[<Fact>]
let ``Json.Optic.get returns correct values`` () =
    Optic.get id_ t1 =! Ok t1

[<Fact>]
let ``Json.Optic.get with Lens returns correct values`` () =
    Optic.get prism_ t1 =! Ok true

[<Fact>]
let ``Json.Optic.set with Lens returns correct values`` () =
    Optic.set id_ (Json.infer false) t1 =! Json.infer false

[<Fact>]
let ``Json.Optic.set with Prism returns correct values`` () =
    let json =
        Optic.set prism_ false t1
        |> function
        | Object o -> JsonObject.optimizeWrite o |> JsonObject.toJson
        | json -> json
    json =! t2

(* Parsing *)

[<Fact>]
let ``Json.parse returns correct values`` () =
    Json.parseOrThrow "\"hello\"" =! Json.infer "hello"
    Json.parseOrThrow "\"\"" =! Json.infer ""
    Json.parseOrThrow "\"\\n\"" =! Json.infer "\n"
    Json.parseOrThrow "\"\\u005c\"" =! Json.infer "\\"
    Json.parseOrThrow "\"푟\"" =! Json.infer "푟"

[<Fact>]
let ``Json.tryParse doesn't throw exceptions``() =
    Json.tryParse null =! Error [NoInput]

(* Formatting *)

[<Fact>]
let ``Json.format returns correct values`` () =
    (* String *)
    Json.format <| Json.infer  "hello" =! "\"hello\""

    (* Awkward string *)
    Json.format <| Json.infer  "he\nllo" =! "\"he\\nllo\""

    (* Complex type *)
    Json.format t1 =! """{"bool":true,"number":2}"""

    Json.format (Json.infer  "hello") =! "\"hello\""
    Json.format (Json.infer  "") =! "\"\""
    Json.format (Json.infer  "푟") =! "\"푟\""
    Json.format (Json.infer  "\t") =! "\"\\t\""

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

[<Fact>]
let ``Json.deserialize simple types returns correct values`` () =

    (* Boolean *)

    Json.deserialize (Json.infer true) =! Ok true

    (* Numeric *)

    let fortytwo = Json.infer 42

    Json.deserialize fortytwo =! Ok (decimal 42)
    Json.deserialize fortytwo =! Ok (float 42)
    Json.deserialize fortytwo =! Ok (int 42)
    Json.deserialize fortytwo =! Ok (int16 42)
    Json.deserialize fortytwo =! Ok (int64 42)
    Json.deserialize fortytwo =! Ok (single 42)
    Json.deserialize fortytwo =! Ok (uint16 42)
    Json.deserialize fortytwo =! Ok (uint32 42)
    Json.deserialize fortytwo =! Ok (uint64 42)

    (* String *)

    Json.deserialize (Json.infer "hello") =! Ok "hello"

    (* DateTime *)

    Json.deserialize (Json.infer "Fri, 20 Feb 2015 14:36:21 GMT") =! Ok (DateTime (2015, 2, 20, 14, 36, 21, DateTimeKind.Utc))

    (* DateTimeOffset *)

    Json.deserialize (Json.infer "2015-04-15T13:45:55Z") =! Ok (DateTimeOffset (2015, 4, 15, 13, 45, 55, TimeSpan.Zero))

[<Fact>]
let ``Json.deserialize complex types returns correct values`` () =

    (* Arrays *)

    let x : JsonResult<string array> = Json.deserialize (Json.infer [ Json.infer "hello"; Json.infer "world"])
    x =! Ok [| "hello"; "world" |]

    (* Lists *)

    Json.deserialize (Json.infer [ Json.infer "hello"; Json.infer "world"])
        =! Ok [ "hello"; "world" ]

    (* Maps *)

    Json.deserialize (
        Json.infer
            [ "one", Json.infer 1M
              "two", Json.infer 2M ])
        =! Ok (Map.ofList [ "one", 1; "two", 2 ])

    (* Sets *)

    Json.deserialize (Json.infer [ Json.infer "one"; Json.infer "two" ])
        =! Ok (set [ "one"; "two" ])

    (* Options *)

    Json.deserialize (Json.infer "hello")
        =! Ok (Some "hello")

    (* Tuples *)

    Json.deserialize (Json.infer [ Json.infer "hello"; Json.infer 42M ])
        =! Ok ("hello", 42)

    Json.deserialize (Json.infer [ Json.infer "hello"; Json.infer 42M; Json.infer true ])
        =! Ok ("hello", 42, true)

    Json.deserialize (Json.infer [ Json.infer "hello"; Json.infer 42M; Json.infer true; Json.infer 12M ])
        =! Ok ("hello", 42, true, 12)

let inline (<!>) a b = ObjectReader.Operators.(<!>) a b
let inline (<*>) a b = ObjectReader.Operators.(<*>) a b

type Test =
    { String: string
      Number : int option
      Values: bool list
      Json: Json }

    static member FromJson (_: Test) = jsonReader {
        let! s = JsonObject.read "string"
        let! n = JsonObject.readOpt "number"
        let! v = JsonObject.read "values"
        let! j = JsonObject.read "json"
        return { String = s; Number = n; Values = v; Json = j }
    }
    // static member FromJson (_: Test) =
    //     let make s n v j =
    //         { String = s
    //           Number = n
    //           Values = v
    //           Json = j }
    //     (make
    //     <!> JsonObject.read "string"
    //     <*> JsonObject.readOpt "number"
    //     <*> JsonObject.read "values"
    //     <*> JsonObject.read "json")
    //     |> ObjectReader.toJsonReader

    static member ToJson (x: Test) =
        JsonObject.empty
        |> JsonObject.write "string" x.String
        |> JsonObject.writeOpt "number" x.Number
        |> JsonObject.write "values" x.Values
        |> JsonObject.write "json" x.Json
        |> Json.infer

let testJson =
    Json.infer
        [ "string", Json.infer "hello"
          "number", Json.infer 42M
          "values", Json.infer [ Json.infer true; Json.infer false ]
          "json", Json.infer [ "hello", Json.infer "world" ] ]

let testInstance =
    { String = "hello"
      Number = Some 42
      Values = [ true; false ]
      Json = Json.infer [ "hello", Json.infer "world" ] }

[<Fact>]
let ``Json.deserialize with custom typed returns correct values`` () =
    Json.deserialize testJson =! Ok testInstance

let testJsonWithNullOption =
    Json.infer
        [ "string", Json.infer "hello"
          "number", Json.``null``
          "values", Json.ofList []
          "json", Json.infer [ "hello", Json.infer "world" ] ]

let testInstanceWithNoneOption =
    { String = "hello"
      Number = None
      Values = [ ]
      Json = Json.infer [ "hello", Json.infer "world" ] }

// [<Fact>]
// let ``Json.deserialize with null option value`` () =
//     Json.deserialize testJsonWithNullOption =! Ok testInstanceWithNoneOption

let testJsonWithMissingOption =
    Json.infer
        [ "string", Json.infer "hello"
          "values", Json.ofList []
          "json", Json.infer [ "hello", Json.infer "world" ] ]

[<Fact>]
let ``Json.deserialize with missing value`` () =
    Json.deserialize testJsonWithMissingOption =! Ok testInstanceWithNoneOption

[<Fact>]
let ``Json.serialize with default value`` () =
    Json.serialize testInstanceWithNoneOption =! testJsonWithMissingOption

let testJsonWithNoValues =
    Json.infer
        [ "string", Json.infer "hello"
          "number", Json.infer 42M
          "json", Json.infer [ "hello", Json.infer "world" ] ]

[<Fact>]
let ``Json.deserialize with invalid value includes missing member name`` () =
    let x : JsonResult<Test> = Json.deserialize testJsonWithNoValues
    x =! Error [Tagged ("values", PropertyNotFound)]

[<Fact>]
let ``Json.serialize with simple types returns correct values`` () =

    (* Bool *)

    Json.serialize true =! Json.infer true

    (* Numeric *)

    Json.serialize (decimal 42) =! Json.infer 42M
    Json.serialize (float 42) =! Json.infer 42M
    Json.serialize (int 42) =! Json.infer 42M
    Json.serialize (int16 42) =! Json.infer 42M
    Json.serialize (int64 42) =! Json.infer 42M
    Json.serialize (single 42) =! Json.infer 42M
    Json.serialize (uint16 42) =! Json.infer 42M
    Json.serialize (uint32 42) =! Json.infer 42M
    Json.serialize (uint64 42) =! Json.infer 42M

    (* DateTime *)

    Json.serialize (DateTime (2015, 2, 20, 14, 36, 21, DateTimeKind.Utc)) =! Json.infer "2015-02-20T14:36:21.0000000Z"

    (* DateTimeOffset *)

    Json.serialize (DateTimeOffset (2015, 2, 20, 14, 36, 21, TimeSpan.Zero)) =! Json.infer "2015-02-20T14:36:21.0000000+00:00"

    (* String *)

    Json.serialize "hello" =! Json.infer "hello"

[<Fact>]
let ``Json.serialize with custom types returns correct values`` () =
    Json.serialize testInstance =! testJson

type TestUnion =
    | One of string
    | Two of int * bool

    static member FromJson (_ : TestUnion) : JsonReader<TestUnion> =
      function
      | Property "one" str as json -> Ok (One str)
      | Property "two" (i, b) as json -> Ok (Two (i, b))
      | json -> JsonResult.deserializationError "expected either 'one' or 'two', but found neither"

    static member ToJson (x: TestUnion) =
        let f =
            match x with
            | One (s) -> JsonObject.write "one" s
            | Two (i, b) -> JsonObject.write "two" (i, b)
        f JsonObject.empty |> JsonObject.toJson

let testUnion =
    Two (42, true)

let testUnionJson =
    Json.infer
        [ "two", Json.infer [ Json.infer 42M; Json.infer true ] ]

[<Fact>]
let ``Json.serialize with union types remains tractable`` () =
    Json.serialize testUnion =! testUnionJson

[<Fact>]
let ``Json.deserialize with union types remains tractable`` () =
    Json.deserialize testUnionJson =! Ok testUnion

[<Fact>]
let ``Json.format escapes object keys correctly`` () =
    let data = Map [ "\u001f", "abc" ]
    let serialized = Json.serialize data
    let formatted = Json.format serialized

    formatted =! """{"\u001F":"abc"}"""

module Json =
    let ofDayOfWeek (e : DayOfWeek) =
        e.ToString "G"
        |> Json.infer
    let toDayOfWeek : JsonReader<DayOfWeek> =
        Chiron.Optic.get Json.Optics.String_
        >> Result.bind (JsonResult.withThrowingParser (fun s -> Enum.Parse(typeof<DayOfWeek>, s) :?> DayOfWeek))

type MyDayOfWeekObject =
    { Bool : bool
      Day : DayOfWeek }
    static member ToJson (x:MyDayOfWeekObject) =
        JsonObject.empty
        |> JsonObject.write "bool" x.Bool
        |> JsonObject.writeWith Json.ofDayOfWeek "day_of_week" x.Day
        |> JsonObject.toJson
    static member FromJson (_:MyDayOfWeekObject) =
        (fun b d -> { Bool = b; Day = d }
        <!> JsonObject.read "bool"
        <*> JsonObject.readWith Json.toDayOfWeek "day_of_week")
        |> ObjectReader.toJsonReader

let deserializedMonday = { Bool = true; Day = DayOfWeek.Monday }
let serializedMonday = Json.infer ["bool", Json.infer true; "day_of_week", Json.infer "Monday"]

[<Fact>]
let ``Json.readWith allows using a custom deserialization function`` () =
    Json.deserialize serializedMonday =! Ok deserializedMonday

[<Fact>]
let ``Json.writeWith allows using a custom serialization function`` () =
    Json.serialize deserializedMonday =! serializedMonday


// let testWriter x = jsonWriter {
//     do! JsonObject.write "bool" x.Bool
//     do! JsonObject.writeWith Json.ofDayOfWeek "day_of_week" x.Day
// }
// [<Fact>]
// let ``Json.writeWith allows using a custom serialization function`` () =
//     testWriter deserializedMonday =! serializedMonday


[<Fact>]
let ``Parsing preserves order of array elements`` () =
    test <@ """["1","2","3"]""" |> Json.parseOrThrow = Json.ofList [ Json.ofString "1"; Json.ofString "2"; Json.ofString "3" ] @>

[<Fact>]
let ``Deserialization preserves order of array elements`` () =
    let deserialize = Json.deserialize
    test <@ """["1","2","3"]""" |> Json.parseOrThrow |> deserialize = Ok ["1"; "2"; "3"] @>

[<Fact>]
let ``Formatting preserves order of array elements`` () =
    test <@ Json.ofList [ Json.ofString "1"; Json.ofString "2"; Json.ofString "3" ] |> Json.format = """["1","2","3"]""" @>

[<Fact>]
let ``Serialization preserves order of array elements`` () =
    let serialize = Json.serialize
    test <@ ["1";"2";"3"]|> serialize |> Json.format = """["1","2","3"]""" @>

type Testing =
    { one: int option
      two: bool
      three: int }
    static member Encode1 (x: Testing) =
           JsonObject.add "1" Json.``null``
        >> JsonObject.add "2" (Json.ofBool x.two)
        >> JsonObject.add "3" (Json.ofInt32 x.three)
    static member Encode (x: Testing, jObj) =
        JsonObject.add "1" Json.``null`` jObj
        |> JsonObject.add "2" (Json.ofBool x.two)
        |> JsonObject.add "3" (Json.ofInt32 x.three)

    static member ToJson (x:Testing) =
        JsonObject.empty
        |> JsonObject.add "1" Json.``null``
        |> JsonObject.add "2" (Json.ofBool x.two)
        |> JsonObject.add "3" (Json.ofInt32 x.three)
        |> JsonObject.toJson
    static member FromJson (_:Testing) : JsonReader<Testing> = jsonReader {
        return!
            fun o t r -> { one = o; two = t; three = r }
            <!> JsonObject.read "1"
            <*> JsonObject.read "2"
            <*> JsonObject.read "3"
    }

let testJsonObject = JsonObject.toJson (JsonObject.ofPropertyList [ "1", Json.``null``; "2", Json.ofBool true; "3", Json.ofInt32 42 ])
let testObject = { one = None; two = true; three = 42 }
let [<Literal>] testJson1 = """{"1":null,"2":true,"3":42}"""

[<Fact>]
let ``Encode works?`` () =
    let encode = Json.encode
    test <@ testJsonObject = encode testObject @>

[<Fact>]
let ``Parsing reverses order of object elements`` () =
    test <@ testJson1 |> Json.parseOrThrow = testJsonObject @>

[<Fact>]
let ``Deserialization preserves order of object elements`` () =
    let deserialize = Json.deserialize
    test <@ testJson1 |> Json.parseOrThrow |> deserialize = Ok testObject @>

[<Fact>]
let ``Formatting preserves order of object elements`` () =
    test <@ testJsonObject |> Json.format = testJson1 @>

[<Fact>]
let ``Serialization preserves order of object elements`` () =
    let serialize = Json.serialize
    test <@ testObject |> serialize |> Json.format = testJson1 @>

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

module Encoders =
    module WrapperType =
        let toJson (Sad x) = Json.ofString x
        let fromJson = Optic.get Json.Optics.String_ >> Result.map Sad
    module MixinType =
        let mk f = { f = f }
        let encode x = JsonObject.add "f" (Json.ofListWith Json.ofInt32 x.f)
        let decode = mk <!> JsonObject.read "f"
    module ChildType =
        let mk d e = { d = d; e = e }
        let encode x =
            JsonObject.add "d" (WrapperType.toJson x.d)
         >> JsonObject.add "e" (Json.ofBytes x.e)
        let decode =
            mk
            <!> JsonObject.readWith WrapperType.fromJson "d"
            <*> JsonObject.read "e"
    module ComplexType =
        let mk a b c = { a = a; b = b; c = c }
        let encode x =
            JsonObject.writeOptWith (Json.ofListWith Json.ofInt32) "a" x.a
         >> JsonObject.writeObjWith (ChildType.encode) "b" x.b
         >> JsonObject.mixinObj (MixinType.encode) x.c
        let decode =
            mk
            <!> JsonObject.readOpt "a"
            <*> (JsonObject.tryGetOrFail "b" >> Result.bind (Optic.get Json.Optics.Object_) >> Result.bind ChildType.decode)
            <*> MixinType.decode

type ComplexType with
    static member Encode x = Encoders.ComplexType.encode x
    static member Decode (_:ComplexType) = Encoders.ComplexType.decode

type ChildType with
    static member Encode x = Encoders.ChildType.encode x
    static member Decode (_:ChildType) = Encoders.ChildType.decode

type MixinType with
    static member Encode x = Encoders.MixinType.encode x
    static member Decode (_:MixinType) = Encoders.MixinType.decode

type WrapperType with
    static member ToJson x = Encoders.WrapperType.toJson x
    static member FromJson (_:WrapperType) = Encoders.WrapperType.fromJson

let thing = """{"f":[1,2,3,4],"a":[2,4,6,8],"b":{"e":"SGVsbG8gd29ybGQh","d":"winter"}}"""
let thing2 = """{"a":[2,4,6,8],"b":{"d":"winter","e":"SGVsbG8gd29ybGQh"},"f":[1,2,3,4]}"""
let expected =
    { a = Some [ 2; 4; 6; 8 ]
      b = { d = Sad "winter"
            e = "Hello world!" |> System.Text.Encoding.UTF8.GetBytes }
      c = { f = [ 1; 2; 3; 4 ] } }

[<Fact>]
let ``complexx`` () =
    test <@ Ok expected = (Json.parseOrThrow thing |> Optic.get Json.Optics.Object_ |> Result.bind Encoders.ComplexType.decode) @>
[<Fact>]
let ``complexy`` () =
    test <@ thing2 = (JsonObject.build Encoders.ComplexType.encode expected |> Json.format) @>
