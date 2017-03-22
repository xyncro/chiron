module Chiron.Testing

open FsCheck
open Chiron

let private pair a b = a,b

type NormalSingle = NormalSingle of single with
    member x.Get = match x with NormalSingle v -> v
    override x.ToString () = x.Get.ToString()

type UtcDateTime = UtcDateTime of System.DateTime with
    member x.Get = match x with UtcDateTime v -> v
    override x.ToString () = x.Get.ToString()

let singleArb =
    Arb.from<single>
    |> Arb.filter (fun f -> not (System.Single.IsNaN f || System.Single.IsInfinity f))
    |> Arb.convert NormalSingle (fun (NormalSingle v) -> v)

let utcDateTimeArb =
    Arb.from<System.DateTime>
    |> Arb.convert (fun dt -> dt.ToUniversalTime() |> UtcDateTime) (fun (UtcDateTime dt) -> dt)

let nonNullStringGen =
    Arb.generate<NonNull<string>>
    |> Gen.map (fun (NonNull nes) -> nes)

module Json =
    let jsonNullGen = Gen.constant Json.Null
    let jsonStringGen =
        nonNullStringGen
        |> Gen.map Json.Encode.string
    let jsonDecimalGen =
        Arb.generate<decimal>
        |> Gen.map Json.Encode.decimal
    let jsonFloatGen =
        Arb.generate<NormalFloat>
        |> Gen.map (fun (NormalFloat f) -> f)
        |> Gen.map Json.Encode.float
    let jsonBigIntGen =
        Arb.generate<bigint>
        |> Gen.map Json.Encode.bigint
    let jsonUInt64Gen =
        Arb.generate<uint64>
        |> Gen.map Json.Encode.uint64
    let jsonNumberGen =
        Gen.oneof
            [ jsonUInt64Gen
              jsonDecimalGen
              jsonFloatGen
              jsonBigIntGen ]
    let jsonBoolGen =
        Arb.generate<bool>
        |> Gen.map Json.Encode.bool
    let jsonArrayGen size innerGen =
        Gen.listOfLength size innerGen
        |> Gen.map Json.Encode.list
    let jsonObjectGen size innerGen =
        Gen.map2 pair nonNullStringGen innerGen
        |> Gen.listOfLength size
        |> Gen.map (List.distinctBy fst)
        |> Gen.map Json.Encode.propertyList

    let rec generateSizedJson maxdepth depth =
        let constMult =
            (float (maxdepth - depth) / float maxdepth)
        let calcFreq mult =
            constMult * (float mult)
            |> ceil
            |> int
        fun size -> gen {
            let innerGen = generateSizedJson maxdepth (depth + 1) (size - 1)
            return!
                Gen.frequency
                    [ 1, jsonNullGen
                      1, jsonBoolGen
                      2, jsonStringGen
                      2, jsonNumberGen
                      calcFreq 4, jsonArrayGen size innerGen
                      calcFreq 8, jsonObjectGen size innerGen ]
        }

    let inline ifFullyShrunkThen v shrinkAlt map =
        let shrunk = Arb.shrink v
        if Seq.isEmpty shrunk then
            shrinkAlt
        else
            (shrunk |> Seq.map map)

    let shrinkToNull = Seq.singleton (Json.Null)
    let shrinkToBool = Seq.ofList [ Json.Encode.bool true; Json.Encode.bool false ]
    let shrinkToNumberOrString = Seq.ofList [ Json.Encode.decimal 0M; Json.Encode.string "" ]

    let jsonShrink = function
        | Null -> Seq.empty
        | Bool b ->
            ifFullyShrunkThen b shrinkToNull Json.Encode.bool

        | String s ->
            ifFullyShrunkThen (NonNull s) shrinkToBool (fun (NonNull s) -> Json.Encode.string s)

        | Number n when n.Contains "e" || n.Contains "E" || (n.Contains "." && String.length n > 17) ->
            ifFullyShrunkThen (System.Double.Parse n) shrinkToBool Json.Encode.float

        | Number n when n.Contains "." ->
            ifFullyShrunkThen (System.Decimal.Parse n) shrinkToBool Json.Encode.decimal

        | Number n when String.length n > 18 ->
            ifFullyShrunkThen (System.Numerics.BigInteger.Parse n) shrinkToBool Json.Encode.bigint

        | Number n ->
            ifFullyShrunkThen (System.Int64.Parse n) shrinkToBool Json.Encode.int64

        | Array a ->
            ifFullyShrunkThen a shrinkToNumberOrString Json.Encode.list

        | Object o ->
            ifFullyShrunkThen (JsonObject.toPropertyList o) shrinkToNumberOrString (fun ps -> List.distinctBy fst ps |> Json.Encode.propertyList)

    let jsonGen =
        let getDepthLimit i = ((System.Math.Log (float i,3.)) |> int) + 1
        let sqrtSize i = float i |> sqrt |> int

        Gen.sized <| fun size ->
            let depthLimit = getDepthLimit size
            let size = sqrtSize size

            generateSizedJson depthLimit 0 size

    let jsonArb = Arb.fromGenShrink (jsonGen, jsonShrink)

type Arbitrary = Arbitrary with
    static member NormalSingle () : Arbitrary<NormalSingle> = singleArb
    static member UtcDateTime () : Arbitrary<UtcDateTime> = utcDateTimeArb
    static member Json () : Arbitrary<Json> = Json.jsonArb

module SelfTest =
    open FsCheck.Xunit
    open Swensen.Unquote

    [<Xunit.Fact>]
    let ``Custom arbitrary Json generates sufficiently arbitrary Json`` () =
        let ``Is not an object with a member array containing a fourth item which is a String`` (v : Json) =
            match v with
            | Object m -> JsonObject.toPropertyList m |> List.exists (fun (_, v) -> (match v with Array (_::_::_::(String _)::_::_) -> true | _ -> false)) |> not
            | _ -> true
        raises
            <@ Check.One
              ( { Config.QuickThrowOnFailure with MaxTest=1000; Arbitrary=typeof<Arbitrary> :: Config.Default.Arbitrary },
                ``Is not an object with a member array containing a fourth item which is a String``) @>

    [<Property(Arbitrary=[|typeof<Arbitrary>|])>]
    let rec ``Arbitrary Json doesn't have null strings`` (v : Json) =
        match v with
        | String null -> false
        | Array a -> List.forall ``Arbitrary Json doesn't have null strings`` a
        | Object o -> JsonObject.toPropertyList o |> List.forall (fun (k, v) -> k <> null && ``Arbitrary Json doesn't have null strings`` v)
        | _ -> true
