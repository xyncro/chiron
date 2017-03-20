namespace ChironB.Benchmarks

open Chiron
open BenchmarkDotNet.Attributes
open System.Text

type Testing =
    { one: int option
      two: bool
      three: int }
    static member Encode (x: Testing, jObj: JsonObject): JsonObject =
        jObj
        |> JsonObject.add "1" Json.``null``
        |> JsonObject.add "2" (Json.ofBool x.two)
        |> JsonObject.add "3" (Json.ofInt32 x.three)

[<Config(typeof<CoreConfig>)>]
type Encoder () =
    let testObject = { one = None; two = true; three = 42 }

    [<Benchmark>]
    member x.Encode () =
        Json.encode testObject

module Method1 =
    open Chiron.ObjectReader.Operators

    type ComplexType =
        { a: int list option
          b: ChildType
          c: MixinType }
    and ChildType =
        { d: WrapperType
          e: byte array }
    and MixinType =
        { f: int list }
    and [<Struct>] WrapperType =
        | Sad of string

    module Encoders =
        module WrapperType =
            let toJson (Sad x) = Json.ofString x
            let fromJson = Optic.get Json.Optics.String_ >> Result.map Sad
        module MixinType =
            let mk f = { f = f }
            let encode x = JsonObject.add "f" (Json.ofListWith Json.ofInt32 x.f)
            let decode = mk <!> JsonObject.read "f"
        module ChildType =
            let mk d e = { d = d; e = e }
            let encode x =
                JsonObject.add "d" (WrapperType.toJson x.d)
             >> JsonObject.add "e" (Json.ofBytes x.e)
            let decode =
                mk
                <!> JsonObject.readWith WrapperType.fromJson "d"
                <*> JsonObject.read "e"
        module ComplexType =
            let mk a b c = { a = a; b = b; c = c }
            let encode x =
                JsonObject.writeOptWith (Json.ofListWith Json.ofInt32) "a" x.a
             >> JsonObject.writeObjWith (ChildType.encode) "b" x.b
             >> JsonObject.mixinObj (MixinType.encode) x.c
            let decode =
                mk
                <!> JsonObject.readOpt "a"
                <*> (JsonObject.tryGetOrFail "b" >> Result.bind (Optic.get Json.Optics.Object_) >> Result.bind ChildType.decode)
                <*> MixinType.decode

    type ComplexType with
        static member Encode x = Encoders.ComplexType.encode x
        static member Decode (_:ComplexType) = Encoders.ComplexType.decode

    type ChildType with
        static member Encode x = Encoders.ChildType.encode x
        static member Decode (_:ChildType) = Encoders.ChildType.decode

    type MixinType with
        static member Encode x = Encoders.MixinType.encode x
        static member Decode (_:MixinType) = Encoders.MixinType.decode

    type WrapperType with
        static member ToJson x = Encoders.WrapperType.toJson x
        static member FromJson (_:WrapperType) = Encoders.WrapperType.fromJson

    module Constants =
        let expected =
            { a = Some [ 2; 4; 6; 8 ]
              b = { d = Sad "winter"
                    e = "Hello world!" |> System.Text.Encoding.UTF8.GetBytes }
              c = { f = [ 1; 2; 3; 4 ] } }

module Method2 =
    open Chiron.ObjectReader.Operators

    type ComplexType =
        { a: int list option
          b: ChildType
          c: MixinType }
    and ChildType =
        { d: WrapperType
          e: byte array }
    and MixinType =
        { f: int list }
    and [<Struct>] WrapperType =
        | Sad of string

    type WrapperType with
        static member ToJson (Sad x) = Json.ofString x
        static member FromJson (_:WrapperType) = Optic.get Json.Optics.String_ >> Result.map Sad

    type MixinType with
        static member Encode x = JsonObject.add "f" (Json.ofListWith Json.ofInt32 x.f)
        static member Decode (_:MixinType) = (fun f -> { f = f }) <!> JsonObject.read "f"

    type ChildType with
        static member Encode x =
            JsonObject.add "d" (WrapperType.ToJson x.d)
            >> JsonObject.add "e" (Json.ofBytes x.e)
        static member Decode (_:ChildType) =
            (fun d e -> { d = d; e = e })
            <!> JsonObject.readWith (WrapperType.FromJson Unchecked.defaultof<_>) "d"
            <*> JsonObject.read "e"

    type ComplexType with
        static member Encode x =
            JsonObject.writeOptWith (Json.ofListWith Json.ofInt32) "a" x.a
            >> JsonObject.writeObjWith (ChildType.Encode) "b" x.b
            >> JsonObject.mixinObj (MixinType.Encode) x.c
        static member Decode (_:ComplexType) =
            (fun a b c -> { a = a; b = b; c = c })
            <!> JsonObject.readOpt "a"
            <*> (JsonObject.tryGetOrFail "b" >> Result.bind (Optic.get Json.Optics.Object_) >> Result.bind (ChildType.Decode Unchecked.defaultof<_>))
            <*> (MixinType.Decode Unchecked.defaultof<_>)

    module Constants =
        let expected =
            { a = Some [ 2; 4; 6; 8 ]
              b = { d = Sad "winter"
                    e = "Hello world!" |> System.Text.Encoding.UTF8.GetBytes }
              c = { f = [ 1; 2; 3; 4 ] } }

module Method3 =
    open Chiron.ObjectReader.Operators

    type ComplexType =
        { a: int list option
          b: ChildType
          c: MixinType }
    and ChildType =
        { d: WrapperType
          e: byte array }
    and MixinType =
        { f: int list }
    and [<Struct>] WrapperType =
        | Sad of string

    module Encoders =
        module WrapperType =
            let toJson (Sad x) = Json.ofString x
            let fromJson = Optic.get Json.Optics.String_ >> Result.map Sad
        module MixinType =
            let mk f = { f = f }
            let encode x jobj = JsonObject.add "f" (Json.ofListWith Json.ofInt32 x.f) jobj
            let decode = mk <!> JsonObject.read "f"
        module ChildType =
            let mk d e = { d = d; e = e }
            let encode x jobj =
                jobj
                |> JsonObject.add "d" (WrapperType.toJson x.d)
                |> JsonObject.add "e" (Json.ofBytes x.e)
            let decode =
                mk
                <!> JsonObject.readWith WrapperType.fromJson "d"
                <*> JsonObject.read "e"
        module ComplexType =
            let mk a b c = { a = a; b = b; c = c }
            let encode x jobj =
                jobj
                |> JsonObject.writeOptWith (Json.ofListWith Json.ofInt32) "a" x.a
                |> JsonObject.writeObjWith (ChildType.encode) "b" x.b
                |> JsonObject.mixinObj (MixinType.encode) x.c
            let decode =
                mk
                <!> JsonObject.readOpt "a"
                <*> (JsonObject.tryGetOrFail "b" >> Result.bind (Optic.get Json.Optics.Object_) >> Result.bind ChildType.decode)
                <*> MixinType.decode

    type ComplexType with
        static member Encode x jobj = Encoders.ComplexType.encode x jobj
        static member Decode (_:ComplexType) = Encoders.ComplexType.decode

    type ChildType with
        static member Encode x jobj= Encoders.ChildType.encode x jobj
        static member Decode (_:ChildType) = Encoders.ChildType.decode

    type MixinType with
        static member Encode x jobj = Encoders.MixinType.encode x jobj
        static member Decode (_:MixinType) = Encoders.MixinType.decode

    type WrapperType with
        static member ToJson x = Encoders.WrapperType.toJson x
        static member FromJson (_:WrapperType) = Encoders.WrapperType.fromJson

    module Constants =
        let expected =
            { a = Some [ 2; 4; 6; 8 ]
              b = { d = Sad "winter"
                    e = "Hello world!" |> System.Text.Encoding.UTF8.GetBytes }
              c = { f = [ 1; 2; 3; 4 ] } }

module Constants =
    let thing = """{"f":[1,2,3,4],"a":[2,4,6,8],"b":{"e":"SGVsbG8gd29ybGQh","d":"winter"}}"""
    let thing2 = """{"a":[2,4,6,8],"b":{"d":"winter","e":"SGVsbG8gd29ybGQh"},"f":[1,2,3,4]}"""

[<Config(typeof<CoreConfig>)>]
type Decode () =
    let jObj = Json.parseOrThrow Constants.thing |> Optic.get Json.Optics.Object_ |> function | Ok x -> x; | Error _ -> failwithf "Nope"

    [<Benchmark>]
    member __.Prebuild () : JsonResult<Method1.ComplexType> =
        Method1.ComplexType.Decode Unchecked.defaultof<_> jObj

    [<Benchmark(Baseline=true)>]
    member __.NoPrebuild () : JsonResult<Method2.ComplexType> =
        Method2.ComplexType.Decode Unchecked.defaultof<_> jObj

[<Config(typeof<CoreConfig>)>]
type Encode () =
    [<Benchmark>]
    member __.Prebuild () : Json =
        JsonObject.build Method1.ComplexType.Encode Method1.Constants.expected

    [<Benchmark>]
    member __.PrebuildAlt () : Json =
        Method3.ComplexType.Encode Method3.Constants.expected JsonObject.empty |> JsonObject.toJson

    [<Benchmark(Baseline=true)>]
    member __.NoPrebuild () : Json =
        JsonObject.build Method2.ComplexType.Encode Method2.Constants.expected
