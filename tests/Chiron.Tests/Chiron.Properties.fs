[<FsCheck.Xunit.Properties(Arbitrary=[|typeof<Chiron.Testing.Arbitrary>|])>]
module Chiron.Tests.Properties

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open Chiron
open Chiron.ObjectReader.Operators
open Chiron.Formatting
open Chiron.Parsing
open Chiron.Serialization
open Chiron.Testing

let inline doRoundTripTest (v : 'a) : _ when (^a or ChironDefaults) : (static member ToJson: ^a -> Json) and (^a or ChironDefaults) : (static member FromJson: ^a -> JsonReader<'a>) =
    let (serialize : JsonWriter<'a>),(deserialize : JsonReader<'a>) = Json.serialize, Json.deserialize
    test <@ v |> serialize |> Json.format |> Json.parseOrThrow |> deserialize = Ok v @>

[<Property>]
let ``Non-null strings can be round-tripped`` (NonNull (str : string)) =
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

[<Property>]
let ``Normal single can be round-tripped`` (NormalSingle v) =
    doRoundTripTest v

[<Property>]
let ``Normal float can be round-tripped`` (NormalFloat v) =
    doRoundTripTest v

[<Property>]
let ``decimal can be round-tripped`` (v : decimal) =
    doRoundTripTest v

[<Property>]
let ``bool can be round-tripped`` (v : bool) =
    doRoundTripTest v

[<Property>]
let ``byte array can be round-tripped`` (NonNull (v : byte array)) =
    doRoundTripTest v
    //test <@ v |> Json.ofBytes |> Json.format |> Json.parseOrThrow |> ChironDefaults.FromJson Unchecked.defaultof<byte array> = Ok v @>

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
    test <@ v |> serialize |> Json.format |> Json.parseOrThrow |> deserialize |> Result.map (fun (dt : System.DateTime) -> dt.Kind) = Ok System.DateTimeKind.Utc @>

[<Property>]
let ``DateTimeOffset can be round-tripped`` (v : System.DateTimeOffset) =
    doRoundTripTest v

// [<Property>]
// let ``Json can be round-tripped`` (v : Json) =
//     doRoundTripTest v

type TestRecord =
    { StringField: string
      GuidField: System.Guid option
      DateTimeField: System.DateTimeOffset option
      CharArrayField: System.Char[] }

    static member FromJson (_ : TestRecord) =
        ( fun s g d c -> { StringField = s; GuidField = g; DateTimeField = d; CharArrayField = c }
        <!> JsonObject.read "stringField"
        <*> JsonObject.readOpt "guidField"
        <*> JsonObject.readOpt "dateTimeField"
        <*> JsonObject.readWith (function | String s -> s.ToCharArray() |> Ok | _ -> JsonResult.deserializationError "Sadness") "charArrayField")
        |> ObjectReader.toJsonReader

    static member ToJson { StringField = s; GuidField = g; DateTimeField = d; CharArrayField = c } =
        JsonObject.empty
        |> JsonObject.write "stringField" s
        |> JsonObject.writeOpt "guidField" g
        |> JsonObject.writeOpt "dateTimeField" d
        |> (c |> JsonObject.writeWith (System.String >> Json.ofString) "charArrayField")
        |> JsonObject.toJson

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
        let f =
            match x with
            | CaseWithTwoArgs (a1, a2) -> JsonObject.write "CaseWithTwoArgs" (a1, a2)
            | CaseWithThreeArgs (a1, a2, a3) -> JsonObject.write "CaseWithThreeArgs" (a1, a2, a3)
            | CaseWithFourArgs (a1, a2, a3, a4) -> JsonObject.write "CaseWithFourArgs" (a1, a2, a3, a4)
            | CaseWithFiveArgs (a1, a2, a3, a4, a5) -> JsonObject.write "CaseWithFiveArgs" (a1, a2, a3, a4, a5)
        f JsonObject.empty |> JsonObject.toJson

    static member FromJson (_ : TestUnion) =
        function
        | Property "CaseWithTwoArgs" (a1, a2) as json -> Ok (CaseWithTwoArgs (a1, a2))
        | Property "CaseWithThreeArgs" (a1, a2, a3) as json -> Ok (CaseWithThreeArgs (a1, a2, a3))
        | Property "CaseWithFourArgs" (a1, a2, a3, a4) as json -> Ok (CaseWithFourArgs (a1, a2, a3, a4))
        | Property "CaseWithFiveArgs" (a1, a2, a3, a4, a5) as json -> Ok (CaseWithFiveArgs (a1, a2, a3, a4, a5))
        | json -> JsonResult.deserializationError "Couldn't find `CaseWithXXXArgs`"

[<Property>]
let ``TestUnion can be round-tripped`` (v : TestUnion) =
    (v.StringField <> null) ==> lazy
        doRoundTripTest v
