module Chiron.Tests.Properties

open Chiron
open Xunit
open FsCheck
open FsCheck.Xunit
open Chiron.Operators
open Swensen.Unquote

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

let inline roundTrip (thing : 'a) : 'a =
    Json.serialize thing
    |> Json.format
    |> Json.parse
    |> Json.deserialize

let inline doRoundTripTest v =
    let serialize,deserialize = Json.serialize, Json.deserialize
    test <@ v |> serialize |> Json.format |> Json.parse |> deserialize = v @>

let always x = fun _ -> x
let pair a b = a,b

type NormalSingle = NormalSingle of single with
    member x.Get = match x with NormalSingle v -> v
    override x.ToString () = x.Get.ToString()

type UtcDateTime = UtcDateTime of System.DateTime with
    member x.Get = match x with UtcDateTime v -> v
    override x.ToString () = x.Get.ToString()

type Arbitrary = Arbitrary with
    static member NormalSingle () : Arbitrary<NormalSingle> =
        Arb.from<single>
        |> Arb.filter (fun f -> not (System.Single.IsNaN f || System.Single.IsInfinity f))
        |> Arb.convert NormalSingle (fun (NormalSingle v) -> v)

    static member UtcDateTime () : Arbitrary<UtcDateTime> =
        Arb.from<System.DateTime>
        |> Arb.convert (fun dt -> dt.ToUniversalTime() |> UtcDateTime) (fun (UtcDateTime dt) -> dt)

    static member Json () : Arbitrary<Json> =
        let genNull = Null () |> Gen.constant
        let genNonNullString = Arb.generate<NonNull<string>> |> Gen.map (fun nes -> nes.Get)
        let genJsonString = genNonNullString |> Gen.map String
        let genJsonNumber = Arb.generate<decimal> |> Gen.map Number
        let genJsonBool = Arb.generate<bool> |> Gen.map Bool
        let genJsonArray size innerGen = Gen.listOfLength size innerGen |> Gen.map Array
        let genJsonObject size innerGen = Gen.map2 pair genNonNullString innerGen |> Gen.listOfLength size |> Gen.map (Map.ofList >> Object)

        let sqrtSize = float >> sqrt >> int

        let rec generateSized maxdepth depth =
            fun size -> gen {
              let nextSize = size - 1
              return!
                  Gen.frequency
                      [ 1, genNull
                        1, genJsonBool
                        2, genJsonString
                        2, genJsonNumber
                        ((float (maxdepth - depth) / float maxdepth) * 4. |> ceil |> int), genJsonArray size (generateSized maxdepth (depth + 1) nextSize)
                        ((float (maxdepth - depth) / float maxdepth) * 8. |> ceil |> int), genJsonObject size (generateSized maxdepth (depth + 1) nextSize) ]
            }

        let inline ifFullyShrunkThen v shrinkAlt map =
            let shrunk = Arb.shrink v
            if Seq.isEmpty shrunk then
                shrinkAlt
            else
                (shrunk |> Seq.map map)

        let shrinkToNull = Seq.singleton (Null ())
        let shrinkToBool = Seq.ofList [ Bool true; Bool false ]
        let numberOrString = Seq.ofList [ Number 0M; String "" ]

        let shrink = function
            | Null () -> Seq.empty
            | Bool b -> ifFullyShrunkThen b shrinkToNull Bool
            | String s -> ifFullyShrunkThen s shrinkToBool String
            | Number n -> ifFullyShrunkThen n shrinkToBool Number
            | Array a -> ifFullyShrunkThen a numberOrString Array
            | Object o -> ifFullyShrunkThen o numberOrString Object

        let getDepthLimit = float >> (fun f -> System.Math.Log (f,2.)) >> int

        let gen = Gen.sized <| fun size -> let depthLimit = getDepthLimit size in generateSized (depthLimit + 1) 0 (sqrtSize size)

        Arb.fromGenShrink (gen,shrink)

[<Fact>]
let ``Custom arbitrary Json generates sufficiently arbitrary Json`` () =
    let ``Is not an object with a member array containing a fourth item which is a String`` (v : Json) =
        match v with
        | Object m -> Map.exists (fun _ v -> (match v with Array (_::_::_::(String _)::_::_) -> true | _ -> false)) m |> not
        | _ -> true
    raises
        <@ Check.One
          ( { Config.VerboseThrowOnFailure with MaxTest=1000; Arbitrary=typeof<Arbitrary> :: Config.Default.Arbitrary },
            ``Is not an object with a member array containing a fourth item which is a String``) @>

[<Property(Arbitrary=[|typeof<Arbitrary>|])>]
let rec ``Arbitrary Json doesn't have null strings`` (v : Json) =
    match v with
    | String null -> false
    | Array a -> List.forall ``Arbitrary Json doesn't have null strings`` a
    | Object o -> Map.forall (fun k v -> k <> null && ``Arbitrary Json doesn't have null strings`` v) o
    | _ -> true

[<Arbitrary(typeof<Arbitrary>)>]
module Primitives =
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

//    [<Property>]
//    let ``byte can be round-tripped`` (v : byte) =
//        doRoundTripTest v

//    [<Property>]
//    let ``sbyte can be round-tripped`` (v : sbyte) =
//        doRoundTripTest v

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
        |> Arb.mapFilter (fun dt -> dt.ToUniversalTime()) (fun dt -> dt.Kind = System.DateTimeKind.Utc)

let ``Chiron properties`` () =
    let config = { Config.VerboseThrowOnFailure with
                      Arbitrary = [ typeof<Overrides> ] }
    Check.All<ChironProperties> config