[<FsCheck.Xunit.Arbitrary(typeof<Chiron.Testing.Arbitrary>)>]
module Chiron.Tests.Properties

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open Chiron
open Chiron.Operators
open Chiron.Testing

let inline doRoundTripTest v =
    let serialize,deserialize = Json.serialize, Json.deserialize
    test <@ v |> serialize |> Json.format |> Json.parse |> deserialize = v @>

[<Property>]
let ``Non-null strings can be round-tripped`` (NonNull str) =
    doRoundTripTest str

[<Property>]
let ``Guid can be round-tripped`` (v : System.Guid) =
    doRoundTripTest v

[<Property>]
let ``int16 can be round-tripped`` (v : int16) =
    doRoundTripTest v

[<Property>]
let ``uint16 can be round-tripped`` (v : uint16) =
    doRoundTripTest v

[<Property>]
let ``int can be round-tripped`` (v : int) =
    doRoundTripTest v

[<Property>]
let ``uint32 can be round-tripped`` (v : uint32) =
    doRoundTripTest v

[<Property>]
let ``int64 can be round-tripped`` (v : int64) =
    doRoundTripTest v

[<Property>]
let ``uint64 can be round-tripped`` (v : uint64) =
    doRoundTripTest v

[<Property(Skip="Handling of floating point values is known to fail to round-trip")>]
let ``Normal single can be round-tripped`` (NormalSingle v) =
    doRoundTripTest v

[<Property(Skip="Handling of floating point values is known to fail to round-trip")>]
let ``Normal float can be round-tripped`` (NormalFloat v) =
    doRoundTripTest v

[<Property>]
let ``decimal can be round-tripped`` (v : decimal) =
    doRoundTripTest v

[<Property>]
let ``bool can be round-tripped`` (v : bool) =
    doRoundTripTest v

//[<Property>]
//let ``byte can be round-tripped`` (v : byte) =
//    doRoundTripTest v

//[<Property>]
//let ``sbyte can be round-tripped`` (v : sbyte) =
//    doRoundTripTest v

[<Property>]
let ``UTC DateTime can be round-tripped`` (UtcDateTime v) =
    doRoundTripTest v

[<Property>]
let ``Deserialized DateTimes are UTC`` (v : System.DateTime) =
    let serialize, deserialize = Json.serialize, Json.deserialize
    test <@ (v |> serialize |> Json.format |> Json.parse |> deserialize : System.DateTime).Kind = System.DateTimeKind.Utc @>

[<Property>]
let ``DateTimeOffset can be round-tripped`` (v : System.DateTimeOffset) =
    doRoundTripTest v

[<Property>]
let ``Json can be round-tripped`` (v : Json) =
    doRoundTripTest v

type TestRecord =
    { StringField: string
      GuidField: System.Guid option
      DateTimeField: System.DateTimeOffset option
      CharArrayField: System.Char[] }

    static member FromJson (_ : TestRecord) =
            fun s g d c -> { StringField = s; GuidField = g; DateTimeField = d; CharArrayField = c }
        <!> Json.read "stringField"
        <*> Json.readOrDefault "guidField" None
        <*> Json.tryRead "dateTimeField"
        <*> Json.readWith (function | String s -> s.ToCharArray() |> Value | _ -> Error "Sadness") "charArrayField"

    static member ToJson { StringField = s; GuidField = g; DateTimeField = d; CharArrayField = c } =
            Json.write "stringField" s
         *> Json.writeUnlessDefault "guidField" None g
         *> Json.write "dateTimeField" d
         *> (c |>Json.writeWith (System.String >> String) "charArrayField")

[<Property>]
let ``TestRecord can be round-tripped`` (v : TestRecord) =
    (v.StringField <> null) ==> lazy
        doRoundTripTest v

type TestUnion =
    | CaseWithTwoArgs of string * int
    | CaseWithThreeArgs of string * int * bool
    | CaseWithFourArgs of string * int * bool * System.Guid
    | CaseWithFiveArgs of string * int * bool * System.Guid * System.DateTimeOffset

    member this.StringField =
        match this with
        | CaseWithTwoArgs (s, _) -> s
        | CaseWithThreeArgs (s, _, _) -> s
        | CaseWithFourArgs (s, _, _, _) -> s
        | CaseWithFiveArgs (s, _, _, _, _) -> s

    static member ToJson (x: TestUnion) =
        match x with
        | CaseWithTwoArgs (a1, a2) -> Json.write "CaseWithTwoArgs" (a1, a2)
        | CaseWithThreeArgs (a1, a2, a3) -> Json.write "CaseWithThreeArgs" (a1, a2, a3)
        | CaseWithFourArgs (a1, a2, a3, a4) -> Json.write "CaseWithFourArgs" (a1, a2, a3, a4)
        | CaseWithFiveArgs (a1, a2, a3, a4, a5) -> Json.write "CaseWithFiveArgs" (a1, a2, a3, a4, a5)

    static member FromJson (_ : TestUnion) =
        function
        | Property "CaseWithTwoArgs" (a1, a2) as json -> Json.init (CaseWithTwoArgs (a1, a2)) json
        | Property "CaseWithThreeArgs" (a1, a2, a3) as json -> Json.init (CaseWithThreeArgs (a1, a2, a3)) json
        | Property "CaseWithFourArgs" (a1, a2, a3, a4) as json -> Json.init (CaseWithFourArgs (a1, a2, a3, a4)) json
        | Property "CaseWithFiveArgs" (a1, a2, a3, a4, a5) as json -> Json.init (CaseWithFiveArgs (a1, a2, a3, a4, a5)) json
        | json -> Json.error (sprintf "couldn't deserialise %A" json) json

[<Property>]
let ``TestUnion can be round-tripped`` (v : TestUnion) =
    (v.StringField <> null) ==> lazy
        doRoundTripTest v
