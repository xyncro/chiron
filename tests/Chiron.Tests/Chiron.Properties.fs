module Chiron.Properties

open FsCheck
open NUnit.Framework
open Chiron
open Chiron.Operators

type TestRecord =
    {
        StringField : string
        GuidField : System.Guid
        DateTimeField : System.DateTime
    }
    static member FromJson (_ : TestRecord) =
            fun s g d -> { StringField = s; GuidField = g; DateTimeField = d }
        <!> Json.read "stringField"
        <*> Json.read "guidField"
        <*> Json.read "dateTimeField"
    static member ToJson { StringField = s; GuidField = g; DateTimeField = d } =
        Json.write "stringField" s
        *> Json.write "guidField" g
        *> Json.write "dateTimeField" d

let inline roundTrip (thing : 'a) : 'a =
    Json.serialize thing
    |> Json.format
    |> Json.parse
    |> Json.deserialize

type ChironProperties =
    static member ``Strings can be roundtripped`` (str : string) =
        roundTrip str = str
    static member ``Date times can be roundtripped`` (tr : System.DateTime) =
        roundTrip tr = tr
    static member ``Records can be roundtripped`` (tr : TestRecord) =
        roundTrip tr = tr

type SafeString =
    static member SafeString () =
        Arb.Default.String()
        |> Arb.filter (fun s ->
            s <> null)

[<Test>]
let ``Chiron properties`` () =
    Check.All<ChironProperties>({ Config.VerboseThrowOnFailure with Arbitrary = [typeof<SafeString>] })