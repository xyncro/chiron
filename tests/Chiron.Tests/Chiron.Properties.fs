[<FsCheck.Xunit.Properties(Arbitrary=[|typeof<Chiron.Testing.Arbitrary>|])>]
module Chiron.Tests.Properties

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open Chiron
open Chiron.ObjectReader.Operators
open Chiron.Testing

module E = Json.Encode
module D = Json.Decode

let doRoundTripTestWith (encode: JsonWriter<'a>) (decode: JsonReader<'a>) (v : 'a) =
    test <@ v |> encode |> Json.format |> Json.parse |> JsonResult.bind decode = Ok v @>

let inline doRoundTripTest (v : 'a) : _ when (^a or Inference.Internal.ChironDefaults) : (static member ToJson: ^a -> Json) and (^a or Inference.Internal.ChironDefaults) : (static member FromJson: ^a * Json -> JsonResult<'a>) =
    let (encode : JsonWriter<'a>),(decode : JsonReader<'a>) = Inference.Json.encode, Inference.Json.decode
    doRoundTripTestWith encode decode v

[<Property>]
let ``Non-null strings can be round-tripped`` (NonNull (str : string)) =
    doRoundTripTestWith E.string D.string str

[<Property>]
let ``Guid can be round-tripped`` (v : System.Guid) =
    doRoundTripTestWith E.guid D.guid v

[<Property>]
let ``int16 can be round-tripped`` (v : int16) =
    doRoundTripTestWith E.int16 D.int16 v

[<Property>]
let ``uint16 can be round-tripped`` (v : uint16) =
    doRoundTripTestWith E.uint16 D.uint16 v

[<Property>]
let ``int can be round-tripped`` (v : int) =
    doRoundTripTestWith E.int D.int v

[<Property>]
let ``uint32 can be round-tripped`` (v : uint32) =
    doRoundTripTestWith E.uint32 D.uint32 v

[<Property>]
let ``int64 can be round-tripped`` (v : int64) =
    doRoundTripTestWith E.int64 D.int64 v

[<Property>]
let ``uint64 can be round-tripped`` (v : uint64) =
    doRoundTripTestWith E.uint64 D.uint64 v

[<Property>]
let ``Normal single can be round-tripped`` (NormalSingle v) =
    doRoundTripTestWith E.single D.single v

[<Property>]
let ``Normal float can be round-tripped`` (NormalFloat v) =
    doRoundTripTestWith E.float D.float v

[<Property>]
let ``decimal can be round-tripped`` (v : decimal) =
    doRoundTripTestWith E.decimal D.decimal v

[<Property>]
let ``bool can be round-tripped`` (v : bool) =
    doRoundTripTestWith E.bool D.bool v

[<Property>]
let ``byte array can be round-tripped`` (NonNull (v : byte array)) =
    doRoundTripTestWith E.bytes D.bytes v

//[<Property>]
//let ``byte can be round-tripped`` (v : byte) =
//    doRoundTripTest v

//[<Property>]
//let ``sbyte can be round-tripped`` (v : sbyte) =
//    doRoundTripTest v

[<Property>]
let ``UTC DateTime can be round-tripped`` (UtcDateTime v) =
    doRoundTripTestWith E.dateTime D.dateTime v

[<Property>]
let ``Deserialized DateTimes are UTC`` (v : System.DateTime) =
    test <@ v |> E.dateTime |> Json.format |> Json.parse |> JsonResult.bind D.dateTime |> Result.map (fun (dt : System.DateTime) -> dt.Kind) = Ok System.DateTimeKind.Utc @>

[<Property>]
let ``DateTimeOffset can be round-tripped`` (v : System.DateTimeOffset) =
    doRoundTripTestWith E.dateTimeOffset D.dateTimeOffset v

[<Property>]
let ``Unit can be round-tripped`` () =
    doRoundTripTestWith E.unit D.unit ()

[<Property>]
let ``Json can be round-tripped`` (v : Json) =
    doRoundTripTestWith E.json D.json v

type TestRecord =
    { StringField: string
      GuidField: System.Guid option
      DateTimeField: System.DateTimeOffset option
      CharArrayField: System.Char[] }

module Json =
    module Encode =
        let testRecord x jObj =
            jObj
            |> JsonObject.writeWith E.string "stringField" x.StringField
            |> JsonObject.writeOptionalWith E.guid "guidField" x.GuidField
            |> JsonObject.writeOptionalWith E.dateTimeOffset "dateTimeField" x.DateTimeField
            |> JsonObject.writeWith (fun (cs : char array) -> System.String cs |> E.string) "charArrayField" x.CharArrayField
        let testRecordToJson x =
            JsonObject.buildWith testRecord x
    module Decode =
        let testRecord =
            (fun s g d c -> { StringField = s; GuidField = g; DateTimeField = d; CharArrayField = c })
            <!> JsonObject.readWith D.string "stringField"
            <*> JsonObject.readOptionalWith D.guid "guidField"
            <*> JsonObject.readOptionalWith D.dateTimeOffset "dateTimeField"
            <*> JsonObject.readWith (D.string >> JsonResult.map (fun s -> s.ToCharArray())) "charArrayField"
        let testRecordFromJson =
            ObjectReader.toJsonReader testRecord

type TestRecord with
    static member FromJson (_: TestRecord, json: Json) = Json.Decode.testRecordFromJson json
    static member ToJson (x: TestRecord) = Json.Encode.testRecordToJson x

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
        let f x =
            match x with
            | CaseWithTwoArgs (a1, a2) -> Inference.JsonObject.write "CaseWithTwoArgs" (a1, a2)
            | CaseWithThreeArgs (a1, a2, a3) -> Inference.JsonObject.write "CaseWithThreeArgs" (a1, a2, a3)
            | CaseWithFourArgs (a1, a2, a3, a4) -> Inference.JsonObject.write "CaseWithFourArgs" (a1, a2, a3, a4)
            | CaseWithFiveArgs (a1, a2, a3, a4, a5) -> Inference.JsonObject.write "CaseWithFiveArgs" (a1, a2, a3, a4, a5)
        JsonObject.buildWith f x

    static member FromJson (_ : TestUnion, json: Json) =
        match json with
        | Property "CaseWithTwoArgs" (a1, a2) as json -> Ok (CaseWithTwoArgs (a1, a2))
        | Property "CaseWithThreeArgs" (a1, a2, a3) as json -> Ok (CaseWithThreeArgs (a1, a2, a3))
        | Property "CaseWithFourArgs" (a1, a2, a3, a4) as json -> Ok (CaseWithFourArgs (a1, a2, a3, a4))
        | Property "CaseWithFiveArgs" (a1, a2, a3, a4, a5) as json -> Ok (CaseWithFiveArgs (a1, a2, a3, a4, a5))
        | _ -> JsonResult.deserializationError "Couldn't find `CaseWithXXXArgs`"

[<Property>]
let ``TestUnion can be round-tripped`` (v : TestUnion) =
    (v.StringField <> null) ==> lazy
        doRoundTripTest v
