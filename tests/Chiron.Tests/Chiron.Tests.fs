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
          "number", Number 2. ])

let private t2 =
    Object (Map.ofList
        [ "bool", Bool false
          "number", Number 2. ])

(* Functional

   Tests to exercise the basic functional components of the Json<'a>
   type and combinators thereof. *)

[<Test>]
let ``Json.init returns correct values`` () =
    Json.init 1 t1 =? (Value 1,  t1)

[<Test>]
let ``Json.error returns correct values`` () =
    Json.error "e" t1 =? (Error "e", t1)

[<Test>]
let ``Json.bind returns correct values`` () =
    Json.bind (Json.init 2) (fun x -> Json.init (x * 3)) t1 =? (Value 6, t1)
    Json.bind (Json.error "e") (fun x -> Json.init (x * 3)) t1 =? (Error "e", t1)

[<Test>]
let ``Json.apply returns correct values`` () =
    Json.apply (Json.init (fun x -> x * 3)) (Json.init 2) t1 =? (Value 6, t1)
    Json.apply (Json.init (fun x -> x * 3)) (Json.error "e") t1 =? (Error "e", t1)

[<Test>]
let ``Json.map returns correct values`` () =
    Json.map (fun x -> x * 3) (Json.init 2) t1 =? (Value 6, t1)
    Json.map (fun x -> x * 3) (Json.error "e") t1 =? (Error "e", t1)

[<Test>]
let ``Json.map2 returns correct values`` () =
    Json.map2 (*) (Json.init 2) (Json.init 3) t1 =? (Value 6, t1)
    Json.map2 (*) (Json.error "e") (Json.init 3) t1 =? (Error "e", t1)
    Json.map2 (*) (Json.init 2) (Json.error "e") t1 =? (Error "e", t1)

(* Lens

   Tests to exercise the functional lens based access to Json
   data structures. *)

let private lens =
         idLens 
    <-?> Json.ObjectPIso 
    >??> mapPLens "bool" 
    <??> Json.BoolPIso

[<Test>]
let ``Json.getLens returns correct values`` () =
    Json.getLens idLens t1 =? (Value t1, t1)

[<Test>]
let ``Json.getLensPartial returns correct values`` () =
    Json.getLensPartial lens t1 =? (Value true, t1)

[<Test>]
let ``Json.tryGetLensPartial returns correct values`` () =
    Json.tryGetLensPartial (idLens <-?> Json.NumberPIso) t1 =? (Value None, t1)

[<Test>]
let ``Json.setLens returns correct values`` () =
    Json.setLens idLens (Bool false) t1 =? (Value (), Bool false)

[<Test>]
let ``Json.setLensPartial returns correct values`` () =
    Json.setLensPartial lens false t1 =? (Value (), t2)

[<Test>]
let ``Json.mapLens returns correct values`` () =
    Json.mapLens idLens (fun _ -> Null ()) t1 =? (Value (), Null ())

[<Test>]
let ``Json.mapLensPartial returns correct values`` () =
    Json.mapLensPartial lens not t1 =? (Value (), t2)

(* Parsing *)

[<Test>]
let ``Json.parse returns correct values`` () =
    Json.parse "\"hello\"" =? String "hello"
    Json.parse "\"\"" =? String ""
    Json.parse "\"\\n\"" =? String "\n"
    Json.parse "\"\\u005c\"" =? String "\\"
    Json.parse "\"푟\"" =? String "푟"

(* Formatting *)

[<Test>]
let ``Json.format returns correct values`` () =
    (* String *)
    Json.format <| String "hello" =? "\"hello\""

    (* Awkward string *)
    Json.format <| String "he\nllo" =? "\"he\\nllo\""

    (* Complex type *)
    Json.format t1 =? """{"bool":true,"number":2}"""

    Json.format (String "hello") =? "\"hello\""
    Json.format (String "") =? "\"\""
    Json.format (String "푟") =? "\"푟\""
    Json.format (String "\t") =? "\"\\t\""

(* Mapping

   Tests exercising mapping functions between Json and other F#
   data structures. *)

[<Test>]
let ``Json.deserialize simple types returns correct values`` () =

    (* Boolean *)

    Json.deserialize (Bool true) =? true

    (* Numeric *)

    Json.deserialize (Number 42.) =? decimal 42
    Json.deserialize (Number 42.) =? float 42
    Json.deserialize (Number 42.) =? int 42
    Json.deserialize (Number 42.) =? int16 42
    Json.deserialize (Number 42.) =? int64 42
    Json.deserialize (Number 42.) =? single 42
    Json.deserialize (Number 42.) =? uint16 42
    Json.deserialize (Number 42.) =? uint32 42
    Json.deserialize (Number 42.) =? uint64 42

    (* Numeric as string to maintain accuracy *)

    Json.deserialize (String "42") =? decimal 42

    (* String *)

    Json.deserialize (String "hello") =? "hello"

    (* DateTime *)

    Json.deserialize (String "Fri, 20 Feb 2015 14:36:21 GMT") =? DateTime (2015, 2, 20, 14, 36, 21, DateTimeKind.Utc)

    (* DateTimeOffset *)

    Json.deserialize (String "2015-04-15T13:45:55Z") =? DateTimeOffset (2015, 4, 15, 13, 45, 55, TimeSpan.Zero)

[<Test>]
let ``Json.deserialize complex types returns correct values`` () =

    (* Arrays *)

    Json.deserialize (Array [ String "hello"; String "world"])
        =? [| "hello"; "world" |]

    (* Lists *)

    Json.deserialize (Array [ String "hello"; String "world"])
        =? [ "hello"; "world" ]

    (* Maps *)

    Json.deserialize (
        Object (Map.ofList
            [ "one", Number 1.
              "two", Number 2. ]))
        =? Map.ofList [ "one", 1; "two", 2 ]

    (* Sets *)

    Json.deserialize (Array [ String "one"; String "two" ])
        =? set [ "one"; "two" ]

    (* Options *)

    Json.deserialize (String "hello")
        =? Some "hello"

    (* Tuples *)

    Json.deserialize (Array [ String "hello"; Number 42. ])
        =? ("hello", 42)

type Test =
    { String: string
      Number : int option
      Values: bool list }

    static member FromJson (_: Test) =
            fun s n v ->
                { String = s
                  Number = n
                  Values = v }
        <!> Json.read "string"
        <*> Json.read "number"
        <*> Json.read "values"

    static member ToJson (x: Test) =
            Json.write "string" x.String
         *> Json.write "number" x.Number
         *> Json.write "values" x.Values

let testJson =
    Object (Map.ofList
        [ "string", String "hello"
          "number", Number 42.
          "values", Array [ Bool true; Bool false ] ])

let testInstance =
    { String = "hello"
      Number = Some 42
      Values = [ true; false ] }

[<Test>]
let ``Json.deserialize with custom typed returns correct values`` () =
    Json.deserialize testJson =? testInstance

[<Test>]
let ``Json.serialize with simple types returns correct values`` () =

    (* Bool *)

    Json.serialize true =? Bool true

    (* Numeric *)

    Json.serialize (float 42) =? Number 42.
    Json.serialize (int 42) =? Number 42.
    Json.serialize (int16 42) =? Number 42.
    Json.serialize (int64 42) =? Number 42.
    Json.serialize (single 42) =? Number 42.
    Json.serialize (uint16 42) =? Number 42.
    Json.serialize (uint32 42) =? Number 42.
    Json.serialize (uint64 42) =? Number 42.

    (* Numeric as string to maintain accuracy *)

    Json.serialize (decimal 42) =? String "42"

    (* DateTime *)

    Json.serialize (DateTime (2015, 2, 20, 14, 36, 21, DateTimeKind.Utc)) =? String "2015-02-20T14:36:21.0000000Z"

    (* DateTimeOffset *)

    Json.serialize (DateTimeOffset (2015, 2, 20, 14, 36, 21, TimeSpan.Zero)) =? String "2015-02-20T14:36:21.0000000+00:00"

    (* String *)
    Json.serialize "hello" =? String "hello"

[<Test>]
let ``Json.serialize with custom types returns correct values`` () =
    Json.serialize testInstance =? testJson

type TestUnion =
    | One of string
    | Two of int * bool

    static member ToJson (x: TestUnion) =
        match x with
        | One (s) -> Json.write "one" s
        | Two (i, b) -> Json.write "two" (i, b)

let testUnion =
    Two (42, true)

let testUnionJson =
    Object (Map.ofList
        [ "two", Array [ Number 42.; Bool true ] ])

[<Test>]
let ``Json.serialize with union types remains tractable`` () =
    Json.serialize testUnion =? testUnionJson
