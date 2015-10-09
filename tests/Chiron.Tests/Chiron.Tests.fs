module Chiron.Tests.Functional

open System
open Aether
open Aether.Operators
open Chiron
open Chiron.Operators
open NUnit.Framework
open Swensen.Unquote

(* Cases *)

let private t1 =
    Object (Map.ofList
        [ "bool", Bool true
          "number", Number 2M ])

let private t2 =
    Object (Map.ofList
        [ "bool", Bool false
          "number", Number 2M ])

(* Functional

   Tests to exercise the basic functional components of the Json<'a>
   type and combinators thereof. *)

[<Test>]
let ``Json.init returns correct values`` () =
    Json.init 1 t1 =! (Value 1,  t1)

[<Test>]
let ``Json.error returns correct values`` () =
    Json.error "e" t1 =! (Error "e", t1)

[<Test>]
let ``Json.bind returns correct values`` () =
    Json.bind (Json.init 2) (fun x -> Json.init (x * 3)) t1 =! (Value 6, t1)
    Json.bind (Json.error "e") (fun x -> Json.init (x * 3)) t1 =! (Error "e", t1)

[<Test>]
let ``Json.apply returns correct values`` () =
    Json.apply (Json.init (fun x -> x * 3)) (Json.init 2) t1 =! (Value 6, t1)
    Json.apply (Json.init (fun x -> x * 3)) (Json.error "e") t1 =! (Error "e", t1)

[<Test>]
let ``Json.map returns correct values`` () =
    Json.map (fun x -> x * 3) (Json.init 2) t1 =! (Value 6, t1)
    Json.map (fun x -> x * 3) (Json.error "e") t1 =! (Error "e", t1)

[<Test>]
let ``Json.map2 returns correct values`` () =
    Json.map2 (*) (Json.init 2) (Json.init 3) t1 =! (Value 6, t1)
    Json.map2 (*) (Json.error "e") (Json.init 3) t1 =! (Error "e", t1)
    Json.map2 (*) (Json.init 2) (Json.error "e") t1 =! (Error "e", t1)

(* Lens

   Tests to exercise the functional lens based access to Json
   data structures. *)

let private lens_ =
         Json.Object_
    >??> key_ "bool"
    >??> Json.Bool_

[<Test>]
let ``Json.Lens.get returns correct values`` () =
    Json.Lens.get id_ t1 =! (Value t1, t1)

[<Test>]
let ``Json.Lens.getPartial returns correct values`` () =
    Json.Lens.getPartial lens_ t1 =! (Value true, t1)

[<Test>]
let ``Json.Lens.tryGetPartial returns correct values`` () =
    Json.Lens.tryGetPartial Json.Number_ t1 =! (Value None, t1)

[<Test>]
let ``Json.Lens.set returns correct values`` () =
    Json.Lens.set id_ (Bool false) t1 =! (Value (), Bool false)

[<Test>]
let ``Json.Lens.setPartial returns correct values`` () =
    Json.Lens.setPartial lens_ false t1 =! (Value (), t2)

[<Test>]
let ``Json.Lens.map returns correct values`` () =
    Json.Lens.map id_ (fun _ -> Null ()) t1 =! (Value (), Null ())

[<Test>]
let ``Json.Lens.mapPartial returns correct values`` () =
    Json.Lens.mapPartial lens_ not t1 =! (Value (), t2)

(* Parsing *)

[<Test>]
let ``Json.parse returns correct values`` () =
    Json.parse "\"hello\"" =! String "hello"
    Json.parse "\"\"" =! String ""
    Json.parse "\"\\n\"" =! String "\n"
    Json.parse "\"\\u005c\"" =! String "\\"
    Json.parse "\"푟\"" =! String "푟"

(* Formatting *)

[<Test>]
let ``Json.format returns correct values`` () =
    (* String *)
    Json.format <| String "hello" =! "\"hello\""

    (* Awkward string *)
    Json.format <| String "he\nllo" =! "\"he\\nllo\""

    (* Complex type *)
    Json.format t1 =! """{"bool":true,"number":2}"""

    Json.format (String "hello") =! "\"hello\""
    Json.format (String "") =! "\"\""
    Json.format (String "푟") =! "\"푟\""
    Json.format (String "\t") =! "\"\\t\""

(* Mapping

   Tests exercising mapping functions between Json and other F#
   data structures. *)

[<Test>]
let ``Json.deserialize simple types returns correct values`` () =

    (* Boolean *)

    Json.deserialize (Bool true) =! true

    (* Numeric *)

    Json.deserialize (Number 42M) =! decimal 42
    Json.deserialize (Number 42M) =! float 42
    Json.deserialize (Number 42M) =! int 42
    Json.deserialize (Number 42M) =! int16 42
    Json.deserialize (Number 42M) =! int64 42
    Json.deserialize (Number 42M) =! single 42
    Json.deserialize (Number 42M) =! uint16 42
    Json.deserialize (Number 42M) =! uint32 42
    Json.deserialize (Number 42M) =! uint64 42

    (* String *)

    Json.deserialize (String "hello") =! "hello"

    (* DateTime *)

    Json.deserialize (String "Fri, 20 Feb 2015 14:36:21 GMT") =! DateTime (2015, 2, 20, 14, 36, 21, DateTimeKind.Utc)

    (* DateTimeOffset *)

    Json.deserialize (String "2015-04-15T13:45:55Z") =! DateTimeOffset (2015, 4, 15, 13, 45, 55, TimeSpan.Zero)

[<Test>]
let ``Json.deserialize complex types returns correct values`` () =

    (* Arrays *)

    Json.deserialize (Array [ String "hello"; String "world"])
        =! [| "hello"; "world" |]

    (* Lists *)

    Json.deserialize (Array [ String "hello"; String "world"])
        =! [ "hello"; "world" ]

    (* Maps *)

    Json.deserialize (
        Object (Map.ofList
            [ "one", Number 1M
              "two", Number 2M ]))
        =! Map.ofList [ "one", 1; "two", 2 ]

    (* Sets *)

    Json.deserialize (Array [ String "one"; String "two" ])
        =! set [ "one"; "two" ]

    (* Options *)

    Json.deserialize (String "hello")
        =! Some "hello"

    (* Tuples *)

    Json.deserialize (Array [ String "hello"; Number 42M ])
        =! ("hello", 42)

type Test =
    { String: string
      Number : int option
      Values: bool list
      Json: Json }

    static member FromJson (_: Test) =
            fun s n v j ->
                { String = s
                  Number = n
                  Values = v
                  Json = j }
        <!> Json.read "string"
        <*> Json.readOrDefault "number" None
        <*> Json.read "values"
        <*> Json.read "json"

    static member ToJson (x: Test) =
            Json.write "string" x.String
         *> Json.writeUnlessDefault "number" None x.Number
         *> Json.write "values" x.Values
         *> Json.write "json" x.Json

let testJson =
    Object (Map.ofList
        [ "string", String "hello"
          "number", Number 42M
          "values", Array [ Bool true; Bool false ]
          "json", Object (Map [ "hello", String "world" ]) ])

let testInstance =
    { String = "hello"
      Number = Some 42
      Values = [ true; false ]
      Json = Object (Map [ "hello", String "world" ]) }

[<Test>]
let ``Json.deserialize with custom typed returns correct values`` () =
    Json.deserialize testJson =! testInstance

let testJsonWithNullOption =
    Object (Map.ofList
        [ "string", String "hello"
          "number", Null ()
          "values", Array []
          "json", Object (Map [ "hello", String "world" ]) ])

let testInstanceWithNoneOption =
    { String = "hello"
      Number = None
      Values = [ ]
      Json = Object (Map [ "hello", String "world" ]) }

[<Test>]
let ``Json.deserialize with null option value`` () =
    Json.deserialize testJsonWithNullOption =! testInstanceWithNoneOption

let testJsonWithMissingOption =
    Object (Map.ofList
        [ "string", String "hello"
          "values", Array []
          "json", Object (Map [ "hello", String "world" ]) ])

[<Test>]
let ``Json.deserialize with missing value`` () =
    Json.deserialize testJsonWithMissingOption =! testInstanceWithNoneOption

[<Test>]
let ``Json.serialize with default value`` () =
    Json.serialize testInstanceWithNoneOption =! testJsonWithMissingOption

[<Test>]
let ``Json.serialize with simple types returns correct values`` () =

    (* Bool *)

    Json.serialize true =! Bool true

    (* Numeric *)

    Json.serialize (decimal 42) =! Number 42M
    Json.serialize (float 42) =! Number 42M
    Json.serialize (int 42) =! Number 42M
    Json.serialize (int16 42) =! Number 42M
    Json.serialize (int64 42) =! Number 42M
    Json.serialize (single 42) =! Number 42M
    Json.serialize (uint16 42) =! Number 42M
    Json.serialize (uint32 42) =! Number 42M
    Json.serialize (uint64 42) =! Number 42M

    (* DateTime *)

    Json.serialize (DateTime (2015, 2, 20, 14, 36, 21, DateTimeKind.Utc)) =! String "2015-02-20T14:36:21.0000000Z"

    (* DateTimeOffset *)

    Json.serialize (DateTimeOffset (2015, 2, 20, 14, 36, 21, TimeSpan.Zero)) =! String "2015-02-20T14:36:21.0000000+00:00"

    (* String *)

    Json.serialize "hello" =! String "hello"

[<Test>]
let ``Json.serialize with custom types returns correct values`` () =
    Json.serialize testInstance =! testJson

type TestUnion =
    | One of string
    | Two of int * bool

    static member FromJson (_ : TestUnion) =
      function
      | Property "one" str as json -> Json.init (One str) json
      | Property "two" (i, b) as json -> Json.init (Two (i, b)) json
      | json -> Json.error (sprintf "couldn't deserialise %A to TestUnion" json) json

    static member ToJson (x: TestUnion) =
        match x with
        | One (s) -> Json.write "one" s
        | Two (i, b) -> Json.write "two" (i, b)

let testUnion =
    Two (42, true)

let testUnionJson =
    Object (Map.ofList
        [ "two", Array [ Number 42M; Bool true ] ])

[<Test>]
let ``Json.serialize with union types remains tractable`` () =
    Json.serialize testUnion =! testUnionJson

[<Test>]
let ``Json.deserialize with union types remains tractable`` () =
    Json.deserialize testUnionJson =! testUnion

[<Test>]
let ``Json.format escapes object keys correctly`` () =
    let data = Map [ "\u001f", "abc" ]
    let serialized = Json.serialize data
    let formatted = Json.format serialized

    formatted =! """{"\u001F":"abc"}"""

module Json =
    let ofDayOfWeek (e : DayOfWeek) =
        e.ToString "G"
        |> String
    let toDayOfWeek json =
        match json with
        | String s ->
            match Enum.TryParse<DayOfWeek> s with
            | true, x -> Value x
            | _ -> Error (sprintf "Unable to parse %s as a DayOfWeek" s)
        | _ -> Error (sprintf "Unable to parse %A as a DayOfWeek" json)

type MyDayOfWeekObject =
    { Bool : bool
      Day : DayOfWeek }
    static member ToJson (x:MyDayOfWeekObject) =
           Json.write "bool" x.Bool
        *> Json.writeWith Json.ofDayOfWeek "day_of_week" x.Day
    static member FromJson (_:MyDayOfWeekObject) =
        fun b d -> { Bool = b; Day = d }
        <!> Json.read "bool"
        <*> Json.readWith Json.toDayOfWeek "day_of_week"

let deserializedMonday = { Bool = true; Day = DayOfWeek.Monday }
let serializedMonday = Object (Map.ofList ["bool", Bool true; "day_of_week", String "Monday"])

[<Test>]
let ``Json.readWith allows using a custom deserialization function`` () =
    Json.deserialize serializedMonday =! deserializedMonday

[<Test>]
let ``Json.writeWith allows using a custom serialization function`` () =
    Json.serialize deserializedMonday =! serializedMonday
