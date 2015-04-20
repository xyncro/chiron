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
        let sut = roundTrip str
        sut = str |@ sprintf "(%A should equal %A)" sut str
    static member ``Date times can be roundtripped`` (dt : System.DateTime) =
        let sut = roundTrip dt
        sut = dt |@ sprintf "(%A should equal %A)" sut dt
    static member ``Records can be roundtripped`` (tr : TestRecord) =
        let sut = roundTrip tr
        sut = tr |@ sprintf "(%A should equal %A)" sut tr

type Overrides =
    static member SafeString () =
        Arb.Default.String()
        |> Arb.filter (fun s -> s <> null)

    static member DateTime () =
        Arb.Default.DateTime ()
        |> Arb.filter (fun dt -> dt.Kind = System.DateTimeKind.Utc)

//[<Test>]
let ``Chiron properties`` () =
    let config = { Config.VerboseThrowOnFailure with
                      Arbitrary = [ typeof<Overrides> ] }
    Check.All<ChironProperties> config