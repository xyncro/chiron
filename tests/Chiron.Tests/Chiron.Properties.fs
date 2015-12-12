[<FsCheck.Xunit.Arbitrary(typeof<Chiron.Testing.Arbitrary>)>]
module Chiron.Tests.Properties

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open Chiron
open Chiron.Operators
open Chiron.Testing

type TestRecord =
    { StringField: string
      GuidField: System.Guid
      DateTimeField: System.DateTime }

    static member FromJson (_ : TestRecord) =
            fun s g d -> { StringField = s; GuidField = g; DateTimeField = d }
        <!> Json.read "stringField"
        <*> Json.read "guidField"
        <*> Json.read "dateTimeField"

    static member ToJson { StringField = s; GuidField = g; DateTimeField = d } =
            Json.write "stringField" s
         *> Json.write "guidField" g
         *> Json.write "dateTimeField" d

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
