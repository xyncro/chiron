module Chiron.Testing

open FsCheck
open Chiron

let inline private pair a b = a,b

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

        let getDepthLimit = float >> (fun f -> System.Math.Log (f,3.)) >> int

        let gen = Gen.sized <| fun size -> let depthLimit = getDepthLimit size in generateSized (depthLimit + 1) 0 (sqrtSize size)

        Arb.fromGenShrink (gen,shrink)

module SelfTest =
    open FsCheck.Xunit
    open Swensen.Unquote

    [<Xunit.Fact>]
    let ``Custom arbitrary Json generates sufficiently arbitrary Json`` () =
        let ``Is not an object with a member array containing a fourth item which is a String`` (v : Json) =
            match v with
            | Object m -> Map.exists (fun _ v -> (match v with Array (_::_::_::(String _)::_::_) -> true | _ -> false)) m |> not
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
        | Object o -> Map.forall (fun k v -> k <> null && ``Arbitrary Json doesn't have null strings`` v) o
        | _ -> true
