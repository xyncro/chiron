module ChironZ.NewParser

open System
open System.Globalization
open System.Text
open Aether

(* RFC 7159

   Types, parsers, formatters and other utilities implemented to mirror the
   specification of JSON (JavaScript Object Notation Data Interchange Format)
   as defined in RFC 7159.

   Taken from [http://tools.ietf.org/html/rfc7159] *)

(* Types

   Simple AST for JSON, with included isomorphisms and lenses in Aether form for
   lens/isomorphism based modification of complex JSON structures. *)

[<Struct>]
type Result<'a,'b> =
    | Ok of a:'a
    | Error of b:'b

module Result =
    let bind a2bR = function
        | Ok a -> a2bR a
        | Error b -> Error b

type JsonMemberType =
    | Array
    | Bool
    | Null
    | Number
    | Object
    | String

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonMemberType =
    let describe = function
        | Array -> "an array"
        | Bool -> "a boolean"
        | Null -> "null"
        | Number -> "a number"
        | Object -> "an object"
        | String -> "a string"

type JsonObject =
    | UnmappedObject of propList:(string * Json) list
    | MappedObject of propList:(string * Json) list * propMap:Map<string,Json>

and Json =
    | Array of elements:Json list
    | Bool of bool
    | Null
    | Number of stringValue:string
    | Object of properties:JsonObject
    | String of string

    (* Epimorphisms *)

    // static member internal Array__ =
    //     (function | Array x -> Some x
    //               | _ -> None), Array

    // static member internal Bool__ =
    //     (function | Bool x -> Some x
    //               | _ -> None), Bool

    // static member internal Null__ =
    //     (function | Null -> Some ()
    //               | _ -> None), (fun () -> Null)

    // static member internal Number__ =
    //     (function | Number x -> Some x
    //               | _ -> None), Number

    // static member internal Object__ =
    //     (function | Object x -> Some x
    //               | _ -> None), Object

    // static member internal String__ =
    //     (function | String x -> Some x
    //               | _ -> None), String

    // (* Prisms *)

    // static member Array_ =
    //     Prism.ofEpimorphism Json.Array__

    // static member Bool_ =
    //     Prism.ofEpimorphism Json.Bool__

    // static member Null_ =
    //     Prism.ofEpimorphism Json.Null__

    // static member Number_ =
    //     Prism.ofEpimorphism Json.Number__

    // static member Object_ =
    //     Prism.ofEpimorphism Json.Object__

    // static member String_ =
    //     Prism.ofEpimorphism Json.String__

type JsonFailure =
    | NoInput
    | PropertyNotFound of propertyName: string
    | TypeMismatch of expected: JsonMemberType * actual: JsonMemberType
    | DeserializationError of targetType: System.Type * err: string
    | ParserFailure of parserFail:string
    | Tagged of propertyName: string * failure: JsonFailure

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonFailure =
    let rec toString = function
        | NoInput -> "No input was provided"
        | PropertyNotFound p -> sprintf "Failed to find expected property '%s'" p
        | TypeMismatch (e,a) -> sprintf "Expected to find %s, but instead found %s" (JsonMemberType.describe e) (JsonMemberType.describe a)
        | DeserializationError (t,e) -> sprintf "Unable to deserialize value as '%s': %s" t.FullName e
        | ParserFailure e -> sprintf "Invalid JSON, failed to parse: %s" e
        | Tagged (p, (Tagged _ as f)) -> sprintf "%s.%s" p (toString f)
        | Tagged (p,f) -> sprintf "%s: %s" p (toString f)

    let withTag tag (a2bR : 'a -> Result<'b,JsonFailure list>) (a : 'a) =
        match a2bR a with
        | Ok a -> Ok a
        | Error errs -> List.map (fun e -> Tagged (tag, e)) errs |> Error

[<AutoOpen>]
module Temp =
    let toJsonMemberType = function
        | Json.Object _ -> JsonMemberType.Object
        | Json.Array _ -> JsonMemberType.Array
        | Json.Bool _ -> JsonMemberType.Bool
        | Json.Null -> JsonMemberType.Null
        | Json.Number _ -> JsonMemberType.Number
        | Json.String _ -> JsonMemberType.String

type Json<'a> =
    Json -> Result<'a,JsonFailure list> * Json

type Deserializer<'a> =
    Json -> Result<'a,JsonFailure list>

type ObjectDeserializer<'a> =
    JsonObject -> Result<'a,JsonFailure list>

type Serializer<'a> =
    'a -> Json

module JsonResult =
    let summarize result =
        match result with
        | Ok x -> sprintf "No errors"
        | Error [e] -> sprintf "Found 1 error:\n  %s" <| JsonFailure.toString e
        | Error errs ->
            let sb = System.Text.StringBuilder()
            let sb = sb.AppendLine(sprintf "Found %i errors:" (List.length errs))
            let sb = errs |> List.fold (fun (sb:System.Text.StringBuilder) e -> sb.Append("  ").AppendLine(JsonFailure.toString e)) sb
            sb.ToString()

// type SerializerDummy<'a> = SerializerDummy of 'a with
//     static member Deserialize : Deserializer<string> = function
//         | Json.String str -> Ok str
//         | x -> Error [TypeMismatch (JsonMemberType.String, toJsonMemberType x)]
//     static member Deserialize : Deserializer<int> = function
//         | Json.Number str -> Ok (int32 str)
//         | x -> Error [TypeMismatch (JsonMemberType.String, toJsonMemberType x)]

type SerializationDefaults = DefaultSerializers with
    static member Deserialize (_:string) : Deserializer<string> = function
        | Json.String str -> Ok str
        | x -> Error [TypeMismatch (JsonMemberType.String, toJsonMemberType x)]
    static member Deserialize (_:int) : Deserializer<int> = function
        | Json.Number str ->
            match System.Int32.TryParse (str) with
            | true, x -> Ok x
            | false, _ -> Error [DeserializationError (typeof<int>, "Invalid format")]
        | x -> Error [TypeMismatch (JsonMemberType.Number, toJsonMemberType x)]
    static member Serialize (str: string) =
        Json.String str
    static member Serialize (str: int) =
        Json.Number (string str)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonObject =
    let empty = UnmappedObject []

    let add kvp = function
        | UnmappedObject ps -> UnmappedObject (kvp :: ps)
        | MappedObject (ps, mps) ->
            let newMps = Map.add (fst kvp) (snd kvp) mps
            MappedObject (kvp :: ps, newMps)

    let inline writeWith (serialize : Serializer<'a>) (k : string, v : 'a) (jsonObject : JsonObject) =
        add (k, serialize v) jsonObject

    let inline writeWithDefault (defaults : ^def) (k : string, v : ^a) (jsonObject : JsonObject) =
        add (k, ((^a or ^def) : (static member Serialize : ^a -> Json) v)) jsonObject

    let inline write kvp (jsonObject : JsonObject) =
        writeWithDefault DefaultSerializers kvp jsonObject

    let inline serializeAsObject (objectBuilder : JsonObject -> JsonObject) =
        empty
        |> objectBuilder
        |> Json.Object

    let optimizeForAccess = function
        | UnmappedObject ps -> MappedObject (ps, Map.ofList ps)
        | o -> o

    let optimizeForAppend = function
        | MappedObject (ps, mps) -> UnmappedObject ps
        | o -> o

    let dedupe = function
        | UnmappedObject ps -> UnmappedObject (List.distinctBy fst ps)
        | MappedObject (ps, mps) -> MappedObject (List.distinctBy fst ps, mps)

    let remove k = function
        | UnmappedObject ps -> UnmappedObject (List.filter (fun (k',_) -> k' <> k) ps)
        | MappedObject (ps, mps) -> MappedObject (List.filter (fun (k',_) -> k' <> k) ps, Map.remove k mps)

    let tryGet k = function
        | UnmappedObject ps -> List.tryPick (fun (k',v) -> if k' = k then Some v else None) ps
        | MappedObject (_, mps) -> Map.tryFind k mps

    let tryGetOrFail k jsonObj =
        match tryGet k jsonObj with
        | Some x -> Ok x
        | None -> Error [PropertyNotFound k]

    let inline readWithDeserializer (deserialize : Deserializer<'a>) (k: string) (jsonObject: JsonObject) : Result<'a,JsonFailure list> =
        tryGetOrFail k jsonObject
        |> Result.bind (JsonFailure.withTag k deserialize)

    let inline readWithDefault (defaults : ^def) (k : string) (jsonObject: JsonObject) : Result<'a,JsonFailure list> =
        let deserializer = ((^a or ^def) : (static member Deserialize : ^a -> Deserializer<'a>) Unchecked.defaultof<'a>)

        readWithDeserializer deserializer k jsonObject

    let inline read (k: string) (jsonObject: JsonObject) : Result<'a,JsonFailure list> =
        readWithDefault DefaultSerializers k jsonObject

    let deserialize = function
        | Json.Object str -> Ok str
        | x -> Error [TypeMismatch (JsonMemberType.Object, toJsonMemberType x)]

[<RequireQualifiedAccess>]
module ObjectDeserializer =
    let init (a: 'a) : ObjectDeserializer<'a> =
        fun json -> Ok a

    let error (e: JsonFailure) : ObjectDeserializer<'a> =
        fun json -> Error [e]

    let ofResult result : ObjectDeserializer<_> =
        fun json -> result

    let bind (a2bD: 'a -> ObjectDeserializer<'b>) (aD: ObjectDeserializer<'a>) : ObjectDeserializer<'b> =
        fun json ->
            match aD json with
            | Ok a -> a2bD a json
            | Error es -> Error es

    // let bind (aD: ObjectDeserializer<'a>) (a2bD: 'a -> ObjectDeserializer<'b>) : ObjectDeserializer<'b> =
    //     fun json ->
    //         match aD json with
    //         | Ok a -> a2bD a json
    //         | Error es -> Error es

    let apply (a2Db: ObjectDeserializer<'a -> 'b>) (aD: ObjectDeserializer<'a>) : ObjectDeserializer<'b> =
        fun json ->
            match a2Db json, aD json with
            | Ok a2b, Ok a -> Ok (a2b a)
            | Error e, Ok _
            | Ok _, Error e -> Error e
            | Error [e1], Error [e2] -> Error [e1; e2]
            | Error es1, Error es2 -> Error (es1 @ es2)

    let map (a2b: 'a -> 'b) (aD: ObjectDeserializer<'a>) : ObjectDeserializer<'b> =
        fun json ->
            match aD json with
            | Ok a -> Ok (a2b a)
            | Error e -> Error e

    let map2 (a2b2c: 'a -> 'b -> 'c) (aD: ObjectDeserializer<'a>) (bD: ObjectDeserializer<'b>) : ObjectDeserializer<'c> =
        apply (map a2b2c aD) bD

    let map3 (a2b2c2d: 'a -> 'b -> 'c -> 'd) (aD: ObjectDeserializer<'a>) (bD: ObjectDeserializer<'b>) (cD: ObjectDeserializer<'c>) : ObjectDeserializer<'d> =
        apply (apply (map a2b2c2d aD) bD) cD

    let map4 (a2b2c2d2x: 'a -> 'b -> 'c -> 'd -> 'x) (aD: ObjectDeserializer<'a>) (bD: ObjectDeserializer<'b>) (cD: ObjectDeserializer<'c>) (dD: ObjectDeserializer<'d>) : ObjectDeserializer<'x> =
        apply (apply (apply (map a2b2c2d2x aD) bD) cD) dD

    let map5 (a2b2c2d2x2y: 'a -> 'b -> 'c -> 'd -> 'x -> 'y) (aD: ObjectDeserializer<'a>) (bD: ObjectDeserializer<'b>) (cD: ObjectDeserializer<'c>) (dD: ObjectDeserializer<'d>) (xD: ObjectDeserializer<'x>) : ObjectDeserializer<'y> =
        apply (apply (apply (apply (map a2b2c2d2x2y aD) bD) cD) dD) xD

    let map6 (a2b2c2d2x2y2z: 'a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) (aD: ObjectDeserializer<'a>) (bD: ObjectDeserializer<'b>) (cD: ObjectDeserializer<'c>) (dD: ObjectDeserializer<'d>) (xD: ObjectDeserializer<'x>) (yD: ObjectDeserializer<'y>) : ObjectDeserializer<'z> =
        apply (apply (apply (apply (apply (map a2b2c2d2x2y2z aD) bD) cD) dD) xD) yD

    let toJsonDeserializer (f: ObjectDeserializer<'a>) : Deserializer<'a> = function
        | Json.Object o -> f o
        | x -> Error [TypeMismatch (JsonMemberType.Object, toJsonMemberType x)]

    type JsonObjectDeserializerBuilder() =
        member __.Return (a: 'a) : ObjectDeserializer<'a> = init a
        member __.ReturnFrom (aD: ObjectDeserializer<'a>) = aD
        member __.Bind (aM, a2bM) : ObjectDeserializer<'a> = bind a2bM aM
        member __.Zero () = init ()
        member __.Combine(r1, r2) = r1 |> bind (fun () -> r2)
        member __.Delay(a2bM) = init () |> bind a2bM

        member __.Run (aM : ObjectDeserializer<'a>) : Deserializer<'a> =
            toJsonDeserializer aM

    let jsonObjectReader =
        JsonObjectDeserializerBuilder()


(* Functional

   Functional signatures for working with Json types, implying a monadic
   approach to working with Json where appropriate.

   Additionally includes common functions for combining and creating
   functions of type Json<'a> which may be used via operator based
   combinators or a computation expression (both provided later). *)

    (* Functions

       Common functions for combining Json<'a> functions in to new
       forms, and for creating new Json<'a> functions given suitable
       initial data. *)

[<RequireQualifiedAccess>]
module Deserializer =
    let toJson (aD: Deserializer<'a>) : Json<'a> =
        fun json ->
            aD json, json

    let init (a: 'a) : Deserializer<'a> =
        fun json -> Ok a

    let error (e: JsonFailure) : Deserializer<'a> =
        fun json -> Error [e]

    let ofResult result =
        fun json -> result

    let bind (aD: Deserializer<'a>) (a2bD: 'a -> Deserializer<'b>) : Deserializer<'b> =
        fun json ->
            match aD json with
            | Ok a -> a2bD a json
            | Error es -> Error es

    let apply (a2Db: Deserializer<'a -> 'b>) (aD: Deserializer<'a>) : Deserializer<'b> =
        fun json ->
            match a2Db json, aD json with
            | Ok a2b, Ok a -> Ok (a2b a)
            | Error e, Ok _
            | Ok _, Error e -> Error e
            | Error [e1], Error [e2] -> Error [e1; e2]
            | Error es1, Error es2 -> Error (es1 @ es2)

    let map (a2b: 'a -> 'b) (aD: Deserializer<'a>) : Deserializer<'b> =
        fun json ->
            match aD json with
            | Ok a -> Ok (a2b a)
            | Error e -> Error e

    let map2 (a2b2c: 'a -> 'b -> 'c) (aD: Deserializer<'a>) (bD: Deserializer<'b>) : Deserializer<'c> =
        apply (map a2b2c aD) bD

    let map3 (a2b2c2d: 'a -> 'b -> 'c -> 'd) (aD: Deserializer<'a>) (bD: Deserializer<'b>) (cD: Deserializer<'c>) : Deserializer<'d> =
        apply (apply (map a2b2c2d aD) bD) cD

    let map4 (a2b2c2d2x: 'a -> 'b -> 'c -> 'd -> 'x) (aD: Deserializer<'a>) (bD: Deserializer<'b>) (cD: Deserializer<'c>) (dD: Deserializer<'d>) : Deserializer<'x> =
        apply (apply (apply (map a2b2c2d2x aD) bD) cD) dD

    let map5 (a2b2c2d2x2y: 'a -> 'b -> 'c -> 'd -> 'x -> 'y) (aD: Deserializer<'a>) (bD: Deserializer<'b>) (cD: Deserializer<'c>) (dD: Deserializer<'d>) (xD: Deserializer<'x>) : Deserializer<'y> =
        apply (apply (apply (apply (map a2b2c2d2x2y aD) bD) cD) dD) xD

    let map6 (a2b2c2d2x2y2z: 'a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) (aD: Deserializer<'a>) (bD: Deserializer<'b>) (cD: Deserializer<'c>) (dD: Deserializer<'d>) (xD: Deserializer<'x>) (yD: Deserializer<'y>) : Deserializer<'z> =
        apply (apply (apply (apply (apply (map a2b2c2d2x2y2z aD) bD) cD) dD) xD) yD

    [<RequireQualifiedAccess>]
    module Optic =

        type Get =
            | Get with

            static member (^.) (Get, l: Lens<Json,'b>) : Deserializer<_> =
                fun json ->
                    init (Optic.get l json) json

            static member (^.) (Get, p: Prism<Json,'b>) : Deserializer<_> =
                fun json ->
                    match Optic.get p json with
                    | Some x -> init x json
                    | _ -> error (DeserializationError (typeof<'b>, sprintf "Couldn't use Prism %A on JSON: '%A'" p json)) json

        let inline get o : Deserializer<_> =
            (Get ^. o)

        type TryGet =
            | TryGet with

            static member (^.) (TryGet, l: Lens<Json,'b>) : Deserializer<_> =
                fun json ->
                    init (Some (Optic.get l json)) json

            static member (^.) (TryGet, p: Prism<Json,'b>) : Deserializer<_> =
                fun json ->
                    init (Optic.get p json) json

        let inline tryGet o : Deserializer<_> =
            (TryGet ^. o)

    module Reflected =
        open System.Reflection
        open FSharp.Reflection
        open FSharp.Quotations.Patterns
        open System.Reflection.Emit

        let tryFindOnHostType (host : System.Type) (target: System.Type) =
            let deserializer = host.GetTypeInfo().GetMethod("Deserialize", [|target; typeof<Json>|])
            if isNull deserializer |> not && deserializer.ReturnType = typedefof<Result<_,_>>.MakeGenericType(target, typeof<JsonFailure list>) then
                Some deserializer
            else
                None

        module Option =
            let orElse yO = function
                | Some x -> Some x
                | None -> yO

            let getOrFail str = function
                | Some x -> x
                | None -> failwith str

        let findDeserializerByType (t: System.Type) =
            tryFindOnHostType t t
            |> Option.orElse (tryFindOnHostType typeof<SerializationDefaults> t)
            |> Option.getOrFail (sprintf "Unable to find serializer for %s" t.FullName)

[<RequireQualifiedAccess>]
module Serializer =
    module Reflected =
        open System.Reflection
        open FSharp.Reflection
        open FSharp.Quotations.Patterns
        open System.Reflection.Emit

        let tryFindOnHostType (host : System.Type) (target: System.Type) =
            let serializer = host.GetTypeInfo().GetMethod("Serialize", [|target|])
            if isNull serializer |> not && serializer.ReturnType = typeof<Json> then
                Some serializer
            else
                None

        module Option =
            let orElse yO = function
                | Some x -> Some x
                | None -> yO

            let getOrFail str = function
                | Some x -> x
                | None -> failwith str

        let findSerializerByType (t: System.Type) =
            tryFindOnHostType t t
            |> Option.orElse (tryFindOnHostType typeof<SerializationDefaults> t)
            |> Option.getOrFail (sprintf "Unable to find serializer for %s" t.FullName)

        let recordSerializerV1<'a> : 'a -> Json =
            let (NewUnionCase (newJsonObjectCaseInfo, _)) = <@ Json.Object JsonObject.empty @>
            let newJsonObject = FSharpValue.PreComputeUnionConstructorInfo (newJsonObjectCaseInfo, allowAccessToPrivateRepresentation = true)
            // let (PropertyGet (_, jsonEmpty, _)) = <@ Json.empty @>
            let (PropertyGet (_, jsonObjectEmpty, _)) = <@ JsonObject.empty @>
            let (Call (_, jsonObjectAdd, _)) = <@ JsonObject.add ("x", Json.Object JsonObject.empty) JsonObject.empty @>
            // let (Call (_, mkTuple, _)) = <@ mkKvp "x" Json.empty @>
            let (mkTuple, _) = FSharpValue.PreComputeTupleConstructorInfo (typeof<string * Json>)
            let pis = FSharpType.GetRecordFields(typeof<'a>)
            let cnt = Array.length pis |> uint32
            let dm = DynamicMethod(typeof<'a>.Name + "Serializer", typeof<Json>, [|typeof<'a>|], true)
            // printfn "IsSecurityCritical: %O, IsSecuritySafeCritical: %O, IsSecurityTransparent: %O" dm.IsSecurityCritical dm.IsSecuritySafeCritical dm.IsSecurityTransparent
            let ilgen = dm.GetILGenerator()
            let jsonObject = ilgen.DeclareLocal(typeof<JsonObject>)
            let serJson = ilgen.DeclareLocal(typeof<Json>)
            let folder (i : uint32) (pi : PropertyInfo) =
                ilgen.Emit(OpCodes.Stloc_0)
                let ser = findSerializerByType pi.PropertyType
                ilgen.Emit(OpCodes.Ldarg_0)
                ilgen.Emit(OpCodes.Callvirt, pi.GetMethod)
                ilgen.Emit(OpCodes.Call, ser)
                ilgen.Emit(OpCodes.Stloc_1)
                ilgen.Emit(OpCodes.Ldstr, pi.Name)
                ilgen.Emit(OpCodes.Ldloc_1)
                ilgen.Emit(OpCodes.Ldloc_0)
                ilgen.Emit(OpCodes.Call, jsonObjectAdd)
                i + 1u
            ilgen.Emit(OpCodes.Call, jsonObjectEmpty.GetMethod)
            let _ = Array.fold folder 1u pis
            ilgen.Emit(OpCodes.Tailcall)
            ilgen.Emit(OpCodes.Call, newJsonObject)
            ilgen.Emit(OpCodes.Ret)
            let f = dm.CreateDelegate(typeof<System.Func<'a,Json>>) :?> System.Func<'a,Json>
            fun a -> f.Invoke(a)

        let recordSerializerV2<'a> : 'a -> Json =
            let (NewUnionCase (newJsonObjectCaseInfo, _)) = <@ Json.Object JsonObject.empty @>
            let newJsonObject = FSharpValue.PreComputeUnionConstructorInfo (newJsonObjectCaseInfo, allowAccessToPrivateRepresentation = true)
            let (PropertyGet (_, jsonObjectEmpty, _)) = <@ JsonObject.empty @>
            let (Call (_, jsonObjectAdd, _)) = <@ JsonObject.add ("x", Json.Object JsonObject.empty) JsonObject.empty @>
            let pis = FSharpType.GetRecordFields(typeof<'a>)
            let cnt = Array.length pis |> uint32
            let dm = DynamicMethod(typeof<'a>.Name + "Serializer", typeof<Json>, [|typeof<'a>|], true)
            let ilgen = dm.GetILGenerator()
            let prep (pi : PropertyInfo) () =
                ilgen.Emit(OpCodes.Ldstr, pi.Name)
                ilgen.Emit(OpCodes.Ldarg_0)
                ilgen.Emit(OpCodes.Ldfld, (pi.DeclaringType.GetTypeInfo().GetField(pi.Name + "@", BindingFlags.NonPublic ||| BindingFlags.Instance)))
                ilgen.Emit(OpCodes.Call, findSerializerByType pi.PropertyType)
            let rec finish = function
                | 0u -> ()
                | i -> ilgen.Emit(OpCodes.Call, jsonObjectAdd); finish (i - 1u)
            Array.foldBack prep pis ()
            ilgen.Emit(OpCodes.Call, jsonObjectEmpty.GetMethod)
            finish cnt
            ilgen.Emit(OpCodes.Tailcall)
            ilgen.Emit(OpCodes.Call, newJsonObject)
            ilgen.Emit(OpCodes.Ret)
            let f = dm.CreateDelegate(typeof<System.Func<'a,Json>>) :?> System.Func<'a,Json>
            fun a -> f.Invoke(a)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Json =
    let init (a: 'a) : Json<'a> =
        fun json ->
            (Ok a, json)

    let error (e: string) : Json<'a> =
        fun json ->
            (Error [DeserializationError (typeof<'a>, e)], json)

    let ofResult result =
        fun json ->
            (result, json)

    let bind (aJ: Json<'a>) (a2bJ: 'a -> Json<'b>) : Json<'b> =
        fun json ->
            match aJ json with
            | Ok a, json' -> a2bJ a json'
            | Error e, json' -> Error e, json'

    let apply (a2Jb: Json<'a -> 'b>) (aJ: Json<'a>) : Json<'b> =
        fun json ->
            match a2Jb json with
            | Ok a2b, json' ->
                match aJ json' with
                | Ok a, json'' -> Ok (a2b a), json''
                | Error e, json'' -> Error e, json''
            | Error e, json' -> Error e, json'

    let map (f: 'a -> 'b) (m: Json<'a>) : Json<'b> =
        fun json ->
            match m json with
            | Ok a, json -> Ok (f a), json
            | Error e, json -> Error e, json

    let map2 (a2b2c: 'a -> 'b -> 'c) (aJ: Json<'a>) (bJ: Json<'b>) : Json<'c> =
        apply (map a2b2c aJ) bJ

    let map3 (a2b2c2d: 'a -> 'b -> 'c -> 'd) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) : Json<'d> =
        apply (apply (map a2b2c2d aJ) bJ) cJ

    let map4 (a2b2c2d2x: 'a -> 'b -> 'c -> 'd -> 'x) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) : Json<'x> =
        apply (apply (apply (map a2b2c2d2x aJ) bJ) cJ) dJ

    let map5 (a2b2c2d2x2y: 'a -> 'b -> 'c -> 'd -> 'x -> 'y) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) (xJ: Json<'x>) : Json<'y> =
        apply (apply (apply (apply (map a2b2c2d2x2y aJ) bJ) cJ) dJ) xJ

    let map6 (a2b2c2d2x2y2z: 'a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) (xJ: Json<'x>) (yJ: Json<'y>) : Json<'z> =
        apply (apply (apply (apply (apply (map a2b2c2d2x2y2z aJ) bJ) cJ) dJ) xJ) yJ

    [<RequireQualifiedAccess>]
    module Optic =

        type Get =
            | Get with

            static member (^.) (Get, l: Lens<Json,'b>) : Json<_> =
                fun json ->
                    init (Optic.get l json) json

            static member (^.) (Get, p: Prism<Json,'b>) : Json<_> =
                fun json ->
                    match Optic.get p json with
                    | Some x -> init x json
                    | _ -> error (sprintf "Couldn't use Prism %A on JSON: '%A'" p json) json

        let inline get o : Json<_> =
            (Get ^. o)

        type TryGet =
            | TryGet with

            static member (^.) (TryGet, l: Lens<Json,'b>) : Json<_> =
                fun json ->
                    init (Some (Optic.get l json)) json

            static member (^.) (TryGet, p: Prism<Json,'b>) : Json<_> =
                fun json ->
                    init (Optic.get p json) json

        let inline tryGet o : Json<_> =
            (TryGet ^. o)

        let inline set o v : Json<_> =
            fun json ->
                init () (Optic.set o v json)

        let inline map o f : Json<_> =
            fun json ->
                init () (Optic.map o f json)

    module private Parser =
        open FParsec

        let ws   = spaces // eats any whitespace
        let str s = pstring s

        let stringLiteral : Parser<string,'u> =
            let escape =  anyOf "\"\\/bfnrt"
                        |>> function
                            | 'b' -> "\b"
                            | 'f' -> "\u000C"
                            | 'n' -> "\n"
                            | 'r' -> "\r"
                            | 't' -> "\t"
                            | c   -> string c // every other char is mapped to itself

            let unicodeEscape =
                str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
                    (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                    |> char |> string
                )

            between (str "\"") (str "\"")
                    (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                                (str "\\" >>. (escape <|> unicodeEscape)))



        let jstring = stringLiteral |>> Json.String

        let jsonNumOpts = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowExponent ||| NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowPlusSign
        let jnumber : Parser<Json,'u> =
            fun stream ->
                let reply = numberLiteralE jsonNumOpts (ErrorMessageList(ErrorMessage.Expected("JSON number"))) stream
                if reply.Status = ReplyStatus.Ok then
                    Reply(Json.Number reply.Result.String)
                else
                    Reply(reply.Status, reply.Error)

        let jtrue  = stringReturn "true"  (Json.Bool true)
        let jfalse = stringReturn "false" (Json.Bool false)
        let jnull  = stringReturn "null" Json.Null

        // jvalue, jlist and jobject are three mutually recursive grammar productions.
        // In order to break the cyclic dependency, we make jvalue a parser that
        // forwards all calls to a parser in a reference cell.
        let jvalue, jvalueRef = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser

        let listBetweenStrings sOpen sClose pElement f =
            between (str sOpen) (str sClose) (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

        let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)

        let jlist   = listBetweenStrings "[" "]" jvalue Json.Array
        let toJsonObject = fun lst -> let map = Map.ofList lst in MappedObject (lst,map) |> Json.Object
        let jobject = listBetweenStrings "{" "}" keyValue toJsonObject

        do jvalueRef := choice [jobject
                                jlist
                                jstring
                                jnumber
                                jtrue
                                jfalse
                                jnull]

        let json = ws >>. jvalue .>> ws .>> eof

        let parseJsonString str = run json str

        let parseJsonStream stream =
            runParserOnStream json () "" stream System.Text.Encoding.UTF8

        let handleParserResult = function
            | Success (json, _, _) -> Result.Ok json
            | Failure (e, _, _) -> Result.Error [ParserFailure e]

    let tryParse s =
        if String.IsNullOrWhiteSpace s then
            Error [NoInput]
        else
            Parser.parseJsonString s
            |> Parser.handleParserResult

    let parse =
        tryParse
        >> function | Ok json -> json
                    | Error e -> failwith (List.map JsonFailure.toString e |> String.concat "\n")

    let import s =
        fun json ->
            match tryParse s with
            | Ok json' -> Ok (), json'
            | Error e -> Error e, json

    let parseStream<'stream> =
        Parser.parseJsonStream
        >> Parser.handleParserResult

    let inline deserializeWithDefaults (defaults : ^def) (dummy : ^a): Deserializer<'a> =
        ((^a or ^def) : (static member Deserialize : ^a -> Deserializer<'a>) dummy)

    let inline deserialize (json: Json) =
        deserializeWithDefaults DefaultSerializers Unchecked.defaultof<'a> json

(* Operators

   Symbolic operators for working with Json<'a> functions, providing
   an operator based concise alternative to the primitive Json<'a> combinators
   given as part of Functional.

   This module is not opened by default, as symbolic operators are a matter
   of taste and may also clash with other operators from other libraries. *)

module Operators =

    let inline (>>=) m f =
        Json.bind m f

    let inline (=<<) f m =
        Json.bind m f

    let inline (<*>) f m =
        Json.apply f m

    let inline (<!>) f m =
        Json.map f m

    let inline (>>.) m f =
        Json.bind m (fun _ -> f)

    let inline (.>>) m f =
        Json.bind (fun _ -> m) f

    let inline ( *>) m1 m2 =
        Json.map2 (fun _ x -> x) m1 m2

    let inline ( <*) m1 m2 =
        Json.map2 (fun x _ -> x) m1 m2

    let inline (>=>) m1 m2 =
        Json.bind (fun x -> m1 x) m2

    let inline (<=<) m1 m2 =
        Json.bind (fun x -> m2 x) m1

(* Builder

   Computation expression (builder) for working with JSON structures in a
   simple way, including lensing, morphisms, etc. using the Aether
   library. *)

type JsonBuilder () =

    member __.Bind (m1, m2) : Json<_> =
        Json.bind m1 m2

    member __.Combine (m1, m2) : Json<_> =
        Json.bind m1 (fun () -> m2)

    member __.Delay (f) : Json<_> =
        Json.bind (Json.init ()) f

    member __.Return (x) : Json<_> =
        Json.init x

    member __.ReturnFrom (f) : Json<_> =
        f

    member __.Zero () : Json<_> =
        Json.init ()

let json =
    JsonBuilder ()

type DeserializerBuilder () =

    member __.Bind (m1, m2) : Deserializer<_> =
        Deserializer.bind m1 m2

    member __.Combine (m1, m2) : Deserializer<_> =
        Deserializer.bind m1 (fun () -> m2)

    member __.Delay (f) : Deserializer<_> =
        Deserializer.bind (Deserializer.init ()) f

    member __.Return (x) : Deserializer<_> =
        Deserializer.init x

    member __.ReturnFrom (f) : Deserializer<_> =
        f

    member __.Zero () : Deserializer<_> =
        Deserializer.init ()

let deserialize =
    DeserializerBuilder ()


// (* Escaping

//    Functions for escaped string parsing and formatting, as a
//    minimal encoding function (escaping only disallowed codepoints,
//    but normalizing any input). *)

[<RequireQualifiedAccess>]
module Formatting =
    open System.Text

    type Formatter<'a> =
        'a -> StringBuilder -> StringBuilder

    let inline append (s: string) (sb: StringBuilder) =
        sb.Append s
    let inline appendChar (c: char) (sb: StringBuilder) =
        sb.Append c
    let inline appendChars (cs: char array) (sb: StringBuilder) =
        sb.Append cs
    let inline appendCharRep (c: char) (repeats: int) (sb: StringBuilder) =
        sb.Append (c, repeats)

    let escapeChars =
        [| '"'; '\\'; '\n'; '\r'; '\t'; '\b'; '\f'
           '\u0000'; '\u0001'; '\u0002'; '\u0003'
           '\u0004'; '\u0005'; '\u0006'; '\u0007'
           '\u000B'; '\u000E'; '\u000F'
           '\u0010'; '\u0011'; '\u0012'; '\u0013'
           '\u0014'; '\u0015'; '\u0016'; '\u0017'
           '\u0018'; '\u0019'; '\u001A'; '\u001B'
           '\u001C'; '\u001D'; '\u001E'; '\u001F' |]

    let isEscapeChar = function
        | '"' | '\\' -> true
        | c when c >= '\u0000' && c <= '\u001F' -> true
        | _ -> false

    let isEscapeCharPred = System.Predicate<_> isEscapeChar

    let escaped = function
        | '"' -> @"\"""
        | '\\' -> @"\\"
        | '\n' -> @"\n"
        | '\r' -> @"\r"
        | '\t' -> @"\t"
        | '\f' -> @"\f"
        | '\b' -> @"\b"
        | '\u0000' -> @"\u0000"
        | '\u0001' -> @"\u0001"
        | '\u0002' -> @"\u0002"
        | '\u0003' -> @"\u0003"
        | '\u0004' -> @"\u0004"
        | '\u0005' -> @"\u0005"
        | '\u0006' -> @"\u0006"
        | '\u0007' -> @"\u0007"
        | '\u000B' -> @"\u000B"
        | '\u000E' -> @"\u000E"
        | '\u000F' -> @"\u000F"
        | '\u0010' -> @"\u0010"
        | '\u0011' -> @"\u0011"
        | '\u0012' -> @"\u0012"
        | '\u0013' -> @"\u0013"
        | '\u0014' -> @"\u0014"
        | '\u0015' -> @"\u0015"
        | '\u0016' -> @"\u0016"
        | '\u0017' -> @"\u0017"
        | '\u0018' -> @"\u0018"
        | '\u0019' -> @"\u0019"
        | '\u001A' -> @"\u001A"
        | '\u001B' -> @"\u001B"
        | '\u001C' -> @"\u001C"
        | '\u001D' -> @"\u001D"
        | '\u001E' -> @"\u001E"
        | '\u001F' -> @"\u001F"
        | c -> @"\u" + (int c).ToString("X4")

    let unicodePrefix = ['u'; '0'; '0']

    let writeUnicodeEscapePrefix sb =
        appendChar 'u' sb
        |> appendChar '0'
        |> appendChar '0'

    let writeEscapedChar c sb =
        match c with
        | '"' -> appendChar '"' sb
        | '\\' -> appendChar '\\' sb
        | '\n' -> appendChar 'n' sb
        | '\r' -> appendChar 'r' sb
        | '\t' -> appendChar 't' sb
        | '\f' -> appendChar 'f' sb
        | '\b' -> appendChar 'b' sb
        | '\u0000' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '0'
        | '\u0001' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '1'
        | '\u0002' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '2'
        | '\u0003' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '3'
        | '\u0004' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '4'
        | '\u0005' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '5'
        | '\u0006' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '6'
        | '\u0007' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '7'
        | '\u000B' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar 'B'
        | '\u000E' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar 'E'
        | '\u000F' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar 'F'
        | '\u0010' -> writeUnicodeEscapePrefix sb |> appendChar '0' |> appendChar '0'
        | '\u0011' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '1'
        | '\u0012' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '2'
        | '\u0013' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '3'
        | '\u0014' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '4'
        | '\u0015' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '5'
        | '\u0016' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '6'
        | '\u0017' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '7'
        | '\u0018' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '8'
        | '\u0019' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar '9'
        | '\u001A' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar 'A'
        | '\u001B' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar 'B'
        | '\u001C' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar 'C'
        | '\u001D' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar 'D'
        | '\u001E' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar 'E'
        | '\u001F' -> writeUnicodeEscapePrefix sb |> appendChar '1' |> appendChar 'F'
        | c -> appendChar 'u' sb |> append (int c).ToString("X4")

    let writeChar c sb =
        if isEscapeChar c then
            appendChar '\\' sb
            |> writeEscapedChar c
        else appendChar c sb

    let rec findFirstEscape (cs:string) (i: int) =
        if isEscapeChar cs.[i] then
            i
        else if i < String.length cs - 1
            findFirst cs (i + 1)
        else
            -1

    let writeString (cs:string) (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        let rec inner (cs:string) (index:int) (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
            if index < String.length cs then
                let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
                //let nextEscapeIndex = System.Array.FindIndex(cs, int index, isEscapeCharPred)
                if nextEscapeIndex = -1 then
                    sb.Append(cs,index,String.length cs - index)
                else if nextEscapeIndex = index then
                    append (escaped cs.[nextEscapeIndex]) sb
                    |> inner cs (nextEscapeIndex + 1)
                else
                    sb.Append(cs,index,nextEscapeIndex - index)
                    |> append (escaped cs.[nextEscapeIndex])
                    |> inner cs (nextEscapeIndex + 1)
            else sb
        inner cs 0 sb

    let writeStringByChar (cs:string) (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        let rec inner (cs:string) (index:int) (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
            if index < String.length cs then
                writeChar cs.[index] sb
                |> inner cs (index + 1)
        inner cs 0 sb

    // let rec joinElems sep strs (sb:System.Text.StringBuilder) =
    //     match strs with
    //     | [] -> sb
    //     | [str] -> append str sb
    //     | str :: [strs] -> append str sb |> append sep |> joinElems sep strs

    let rec join (f: Formatter<'a>) (sep: string) values (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        match values with
        | [] -> sb
        | [v] -> f v sb
        | v :: vs ->
            join f sep vs sb
            |> append sep
            |> f v

    let rec joinRev (f: Formatter<'a>) (sep: string) values (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        match values with
        | [] -> sb
        | [v] -> f v sb
        | v :: vs ->
            f v sb
            |> append sep
            |> joinRev f sep vs

    (* Options

       Options for formatting, defined as functions for spacing and newline
       formatting appenders. Predefined formats are given as static members
       as a shorthand. *)

    type PropertyNameSpacing =
        | NoSpaceBetweenNameAndValue
        | SpaceBetweenNameAndValue

    type ElementSpacing =
        | NoSpaceBetweenElements
        | SpaceBetweenElements
        | NewLineBetweenElements of indentSpaces:uint32

    type JsonFormattingOptions =
      { PropertyNameSpacing : PropertyNameSpacing
        ElementSpacing : ElementSpacing }

      static member Compact =
        { PropertyNameSpacing = NoSpaceBetweenNameAndValue
          ElementSpacing = NoSpaceBetweenElements }

      static member SingleLine =
        { PropertyNameSpacing = SpaceBetweenNameAndValue
          ElementSpacing = SpaceBetweenElements }

      static member Pretty =
        { PropertyNameSpacing = SpaceBetweenNameAndValue
          ElementSpacing = NewLineBetweenElements 2u }

    let propNameBaseLength = function
        | NoSpaceBetweenNameAndValue -> 3u
        | SpaceBetweenNameAndValue -> 4u

    let elementSpacingLength level = function
        | NoSpaceBetweenElements -> 0u
        | SpaceBetweenElements -> 1u
        | NewLineBetweenElements spaces -> 1u + spaces * level

    let rec calcLength level options agg = function
        | Json.Array elems ->
            let elemCount = List.length elems |> uint32
            let newLevel = level + 1u
            let baseLen = 2u
            let sepLen = elemCount - 1u
            let lineLen = elemCount * (elementSpacingLength newLevel options.ElementSpacing) + (elementSpacingLength level options.ElementSpacing)
            List.fold (fun a e -> calcLength newLevel options a e) (agg + baseLen + sepLen + lineLen) elems
        | Json.Bool true -> agg + 4u
        | Json.Bool false -> agg + 5u
        | Json.Number str -> agg + (String.length str |> uint32)
        | Json.Null -> agg + 4u
        | Json.Object (UnmappedObject props)
        | Json.Object (MappedObject (props,_)) ->
            let propCount = List.length props |> uint32
            let newLevel = level + 1u
            let baseLen = 2u
            let sepLen = propCount - 1u
            let propNameBaseLen = propCount * (propNameBaseLength options.PropertyNameSpacing)
            let lineLen = propCount * (elementSpacingLength newLevel options.ElementSpacing) + (elementSpacingLength level options.ElementSpacing)
            List.fold (fun a (k,v) -> calcLength newLevel options ((String.length k |> uint32) + a) v) (agg + baseLen + propNameBaseLen + sepLen + lineLen) props
        | Json.String s ->
            agg + 2u + (String.length s |> uint32)

    let appendSpaceBetweenElements { ElementSpacing = es } level sb =
        match es, level with
        | NoSpaceBetweenElements, _ -> sb
        | SpaceBetweenElements, _ -> appendChar ' ' sb
        | NewLineBetweenElements _, 0u -> appendChar '\n' sb
        | NewLineBetweenElements s, l -> appendChar '\n' |> appendCharRep ' ' (s * l |> int)


    let addArrayPrefix { ElementSpacing = es } level sb =
        appendChar '[' sb
        |> appendSpaceBetweenElements options level sb

    let addArraySuffix { ElementSpacing = es } level sb =
        match es, level with
        | NoSpaceBetweenElements, _ -> append "]" sb
        | SpaceBetweenElements, _ -> append " ]" sb
        | NewLineBetweenElements 2u, 0u -> append "\n]" sb
        | NewLineBetweenElements 2u, 1u -> append "\n  ]" sb
        | NewLineBetweenElements 2u, 2u -> append "\n    ]" sb
        | NewLineBetweenElements 2u, 3u -> append "\n      ]" sb
        | NewLineBetweenElements 2u, 4u -> append "\n        ]" sb
        | NewLineBetweenElements 2u, 5u -> append "\n          ]" sb
        | NewLineBetweenElements 2u, 6u -> append "\n            ]" sb
        | NewLineBetweenElements 2u, l -> append ("\n" + String.replicate (int l) "  " + "]") sb
        | NewLineBetweenElements spaces, l -> append ("\n" + String.replicate (spaces * l |> int) " " + "]") sb

    let addObjectPrefix { ElementSpacing = es } level sb =
        match es, level with
        | NoSpaceBetweenElements, _ -> append "{" sb
        | SpaceBetweenElements, _ -> append "{ " sb
        | NewLineBetweenElements 2u, 0u -> append "{\n" sb
        | NewLineBetweenElements 2u, 1u -> append "{\n  " sb
        | NewLineBetweenElements 2u, 2u -> append "{\n    " sb
        | NewLineBetweenElements 2u, 3u -> append "{\n      " sb
        | NewLineBetweenElements 2u, 4u -> append "{\n        " sb
        | NewLineBetweenElements 2u, 5u -> append "{\n          " sb
        | NewLineBetweenElements 2u, 6u -> append "{\n            " sb
        | NewLineBetweenElements 2u, l -> append ("{\n" + String.replicate (int l) "  ") sb
        | NewLineBetweenElements spaces, l -> append ("{\n" + String.replicate (spaces * l |> int) " ") sb

    let addObjectSuffix { ElementSpacing = es } level sb =
        match es, level with
        | NoSpaceBetweenElements, _ -> append "}" sb
        | SpaceBetweenElements, _ -> append " }" sb
        | NewLineBetweenElements 2u, 0u -> append "\n}" sb
        | NewLineBetweenElements 2u, 1u -> append "\n  }" sb
        | NewLineBetweenElements 2u, 2u -> append "\n    }" sb
        | NewLineBetweenElements 2u, 3u -> append "\n      }" sb
        | NewLineBetweenElements 2u, 4u -> append "\n        }" sb
        | NewLineBetweenElements 2u, 5u -> append "\n          }" sb
        | NewLineBetweenElements 2u, 6u -> append "\n            }" sb
        | NewLineBetweenElements 2u, l -> append ("\n" + String.replicate (int l) "  " + "}") sb
        | NewLineBetweenElements spaces, l -> append ("\n" + String.replicate (spaces * l |> int) " " + "}") sb

    let addElementSeparator { ElementSpacing = es } level sb =
        match es, level with
        | NoSpaceBetweenElements, _ -> append "," sb
        | SpaceBetweenElements, _ -> append ", " sb
        | NewLineBetweenElements 2u, 0u -> append ",\n" sb
        | NewLineBetweenElements 2u, 1u -> append ",\n  " sb
        | NewLineBetweenElements 2u, 2u -> append ",\n    " sb
        | NewLineBetweenElements 2u, 3u -> append ",\n      " sb
        | NewLineBetweenElements 2u, 4u -> append ",\n        " sb
        | NewLineBetweenElements 2u, 5u -> append ",\n          " sb
        | NewLineBetweenElements 2u, 6u -> append ",\n            " sb
        | NewLineBetweenElements 2u, l -> append (",\n" + String.replicate (int l) "  ") sb
        | NewLineBetweenElements spaces, l -> append (",\n" + String.replicate (spaces * l |> int) " ") sb

    let elementSeparator level { ElementSpacing = es } =
        match es, level with
        | NoSpaceBetweenElements, _ -> System.String.Empty
        | SpaceBetweenElements, _ -> " "
        | NewLineBetweenElements 2u, 0u -> "\n"
        | NewLineBetweenElements 2u, 1u -> "\n  "
        | NewLineBetweenElements 2u, 2u -> "\n    "
        | NewLineBetweenElements 2u, 3u -> "\n      "
        | NewLineBetweenElements 2u, 4u -> "\n        "
        | NewLineBetweenElements 2u, 5u -> "\n          "
        | NewLineBetweenElements 2u, 6u -> "\n            "
        | NewLineBetweenElements 2u, l -> "\n" + String.replicate (int l) "  "
        | NewLineBetweenElements spaces, l -> "\n" + String.replicate (spaces * l |> int) " "

    let addSeparation { ElementSpacing = es } space sb =
        match es with
        | NoSpaceBetweenElements -> sb
        | SpaceBetweenElements
        | NewLineBetweenElements _ -> append space sb

    let appendSpaceBetweenNameAndValue { PropertyNameSpacing = pns } sb =
        match pns with
        | NoSpaceBetweenNameAndValue -> sb
        | SpaceBetweenNameAndValue -> appendChar ' ' sb

    let addPropertyNameSeparator options sb =
        appendChar ':' sb
        |> appendSpaceBetweenNameAndValue options

    let null = ['n'; 'u'; 'l'; 'l']
    let trueChars = ['t'; 'r'; 'u'; 'e']
    let falseChars = ['f'; 'a'; 'l'; 's'; 'e']

    let rec formatJson level options x sb =
        match x with
        | Json.Array elems -> formatArray level options elems sb
        | Json.Bool x -> formatBool x sb
        | Json.Number x -> formatNumber x sb
        | Json.Null -> formatNull sb
        | Json.Object (UnmappedObject o)
        | Json.Object (MappedObject (o,_)) -> formatObject level options o sb
        | Json.String s -> formatString s sb

    and formatArray level options elems sb =
        let newLevel = level + 1u
        appendChar '[' sb
        |> appendSpaceBetweenElements options newLevel
        |> joinArray options newLevel elems
        |> appendSpaceBetweenElements options level
        |> appendChar ']'

    and formatBool b sb =
        if b then
            appendChars trueChars sb
        else
            appendChars falseChars sb

    and formatNumber n sb =
        append n sb

    and formatNull sb =
        appendChars nullChars sb

    and formatProperty options level (k,v) sb =
        formatString k sb
        |> appendChar ':'
        |> appendSpaceBetweenNameAndValue options
        |> formatJson options level v

    and formatObject options level props sb =
        let newLevel = level + 1u
        let prefix = elementSeparator newLevel options
        let separator = "," + prefix
        let suffix = elementSeparator level options
        appendChar '{' sb
        |> appendSpaceBetweenElements options newLevel
        |> joinObject options newLevel elems
        |> appendSpaceBetweenElements options level
        |> appendChar '}'

    and formatString str sb =
        appendChar '"' sb
        |> writeString str
        |> appendChar ''

    let rec joinObject (f: Formatter<'a>) (sep: string) values (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        match values with
        | [] -> sb
        | [v] -> formatProperty options level v sb
        | v :: vs ->
            joinObject options level sb
            |> appendChar ','
            |> appendSpaceBetweenElements options level
            |> formatProperty options level v vs

    and joinArray options level values (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        match values with
        | [] -> sb
        | [v] -> formatJson options level v sb
        | v :: vs ->
            formatJson options level v sb
            |> appendChar ','
            |> appendSpaceBetweenElements options level
            |> joinArray options level vs


    (* Functions *)

    [<RequireQualifiedAccess>]
    module Json =

        let format json =
            StringBuilder ()
            |> formatJson 0u JsonFormattingOptions.Compact json
            |> string

        let formatWith options json =
            StringBuilder ()
            |> formatJson 0u options json
            |> string

    module Array =
        [<Struct>]
        type State =
            { Aggregate : string array
              Position : int }

        let calcStringTokens (s: string) (agg:int) =
            let rec inner (s: string) (index: int) (agg: int) =
                if index <= (String.length s - 1) then
                    let nextEscapeIndex = s.IndexOfAny(escapeChars, index)
                    if nextEscapeIndex = -1 then
                        agg + 1
                    else if nextEscapeIndex = index then
                        inner s (nextEscapeIndex + 1) (agg + 1)
                    else
                        inner s (nextEscapeIndex + 1) (agg + 2)
                else agg
            inner s 0 agg

        let calcTokens json =
            let rec inner agg = function
                | Json.Object (UnmappedObject props)
                | Json.Object (MappedObject (props,_)) ->
                    let propCount = List.length props
                    List.fold (fun a (k,v) -> inner (calcStringTokens k (a + 2)) v) (agg + propCount * 2 + 1) props
                | Json.Array elems ->
                    let elemCount = List.length elems
                    List.fold (fun a e -> inner a e) (agg + elemCount + 1) elems
                | Json.Bool _
                | Json.Number _
                | Json.Null -> agg + 1
                | Json.String s -> calcStringTokens s (agg + 2)
            inner 0 json

        let inline insert (s: string) (agg : string array) (pos : int) =
            agg.[pos] <- s
            pos + 1

        let writeString (s: string) (agg : string array) (pos : int) =
            let rec inner (s: string) (index: int) (agg : string array) (pos : int) =
                if index < (String.length s - 1) then
                    let nextEscapeIndex = s.IndexOfAny(escapeChars, index)
                    if nextEscapeIndex = -1 && index = 0 then
                        insert s agg pos
                    else if nextEscapeIndex = -1 then
                        insert (s.Substring(index, String.length s - index)) agg pos
                    else if nextEscapeIndex = index then
                        insert (escaped s.[nextEscapeIndex]) agg pos
                        |> inner s (nextEscapeIndex + 1) agg
                    else
                        insert (s.Substring(index, String.length s - index)) agg pos
                        |> insert (escaped s.[nextEscapeIndex]) agg
                        |> inner s (nextEscapeIndex + 1) agg
                else pos
            inner s 0 agg pos

        let addArrayPrefix { ElementSpacing = es } level agg pos =
            match es, level with
            | NoSpaceBetweenElements, _ -> insert "[" agg pos
            | SpaceBetweenElements, _ -> insert "[ " agg pos
            | NewLineBetweenElements 2u, 0u -> insert "[\n" agg pos
            | NewLineBetweenElements 2u, 1u -> insert "[\n  " agg pos
            | NewLineBetweenElements 2u, 2u -> insert "[\n    " agg pos
            | NewLineBetweenElements 2u, 3u -> insert "[\n      " agg pos
            | NewLineBetweenElements 2u, 4u -> insert "[\n        " agg pos
            | NewLineBetweenElements 2u, 5u -> insert "[\n          " agg pos
            | NewLineBetweenElements 2u, 6u -> insert "[\n            " agg pos
            | NewLineBetweenElements 2u, l -> insert ("[\n" + String.replicate (int l) "  ") agg pos
            | NewLineBetweenElements spaces, l -> insert ("[\n" + String.replicate (spaces * l |> int) " ") agg pos

        let addArraySuffix { ElementSpacing = es } level agg pos =
            match es, level with
            | NoSpaceBetweenElements, _ -> insert "]" agg pos
            | SpaceBetweenElements, _ -> insert " ]" agg pos
            | NewLineBetweenElements 2u, 0u -> insert "\n]" agg pos
            | NewLineBetweenElements 2u, 1u -> insert "\n  ]" agg pos
            | NewLineBetweenElements 2u, 2u -> insert "\n    ]" agg pos
            | NewLineBetweenElements 2u, 3u -> insert "\n      ]" agg pos
            | NewLineBetweenElements 2u, 4u -> insert "\n        ]" agg pos
            | NewLineBetweenElements 2u, 5u -> insert "\n          ]" agg pos
            | NewLineBetweenElements 2u, 6u -> insert "\n            ]" agg pos
            | NewLineBetweenElements 2u, l -> insert ("\n" + String.replicate (int l) "  " + "]") agg pos
            | NewLineBetweenElements spaces, l -> insert ("\n" + String.replicate (spaces * l |> int) " " + "]") agg pos

        let addObjectPrefix { ElementSpacing = es } level agg pos =
            match es, level with
            | NoSpaceBetweenElements, _ -> insert "{" agg pos
            | SpaceBetweenElements, _ -> insert "{ " agg pos
            | NewLineBetweenElements 2u, 0u -> insert "{\n" agg pos
            | NewLineBetweenElements 2u, 1u -> insert "{\n  " agg pos
            | NewLineBetweenElements 2u, 2u -> insert "{\n    " agg pos
            | NewLineBetweenElements 2u, 3u -> insert "{\n      " agg pos
            | NewLineBetweenElements 2u, 4u -> insert "{\n        " agg pos
            | NewLineBetweenElements 2u, 5u -> insert "{\n          " agg pos
            | NewLineBetweenElements 2u, 6u -> insert "{\n            " agg pos
            | NewLineBetweenElements 2u, l -> insert ("{\n" + String.replicate (int l) "  ") agg pos
            | NewLineBetweenElements spaces, l -> insert ("{\n" + String.replicate (spaces * l |> int) " ") agg pos

        let addObjectSuffix { ElementSpacing = es } level agg pos =
            match es, level with
            | NoSpaceBetweenElements, _ -> insert "}" agg pos
            | SpaceBetweenElements, _ -> insert " }" agg pos
            | NewLineBetweenElements 2u, 0u -> insert "\n}" agg pos
            | NewLineBetweenElements 2u, 1u -> insert "\n  }" agg pos
            | NewLineBetweenElements 2u, 2u -> insert "\n    }" agg pos
            | NewLineBetweenElements 2u, 3u -> insert "\n      }" agg pos
            | NewLineBetweenElements 2u, 4u -> insert "\n        }" agg pos
            | NewLineBetweenElements 2u, 5u -> insert "\n          }" agg pos
            | NewLineBetweenElements 2u, 6u -> insert "\n            }" agg pos
            | NewLineBetweenElements 2u, l -> insert ("\n" + String.replicate (int l) "  " + "}") agg pos
            | NewLineBetweenElements spaces, l -> insert ("\n" + String.replicate (spaces * l |> int) " " + "}") agg pos

        let addElementSeparator { ElementSpacing = es } level agg pos =
            match es, level with
            | NoSpaceBetweenElements, _ -> insert "," agg pos
            | SpaceBetweenElements, _ -> insert ", " agg pos
            | NewLineBetweenElements 2u, 0u -> insert ",\n" agg pos
            | NewLineBetweenElements 2u, 1u -> insert ",\n  " agg pos
            | NewLineBetweenElements 2u, 2u -> insert ",\n    " agg pos
            | NewLineBetweenElements 2u, 3u -> insert ",\n      " agg pos
            | NewLineBetweenElements 2u, 4u -> insert ",\n        " agg pos
            | NewLineBetweenElements 2u, 5u -> insert ",\n          " agg pos
            | NewLineBetweenElements 2u, 6u -> insert ",\n            " agg pos
            | NewLineBetweenElements 2u, l -> insert (",\n" + String.replicate (int l) "  ") agg pos
            | NewLineBetweenElements spaces, l -> insert (",\n" + String.replicate (spaces * l |> int) " ") agg pos

        let addPropertyNameSeparator { PropertyNameSpacing = pns } agg pos =
            match pns with
            | NoSpaceBetweenNameAndValue -> insert ":" agg pos
            | SpaceBetweenNameAndValue -> insert ": " agg pos

        let rec formatJson options level x agg pos =
            match x with
            | Json.Array elems -> formatArray options level elems agg pos
            | Json.Bool x -> formatBool x agg pos
            | Json.Number x -> formatNumber x agg pos
            | Json.Null -> formatNull agg pos
            | Json.Object (UnmappedObject o)
            | Json.Object (MappedObject (o,_)) -> formatObject options level o agg pos
            | Json.String s -> formatString s agg pos

        and formatArray options level elems agg pos =
            let newLevel = level + 1u
            addArrayPrefix options newLevel agg pos
            |> joinArray options newLevel elems agg
            |> addArraySuffix options level agg

        and formatBool b agg pos =
            let str =
                if b then "true" else "false"
            insert str agg pos

        and formatNumber n agg pos =
            insert n agg pos

        and formatNull agg pos =
            insert "null" agg pos

        and formatProperty options level (k,v) agg pos =
            formatString k agg pos
            |> addPropertyNameSeparator options agg
            |> formatJson options level v agg

        and formatObject options level props agg pos =
            let newLevel = level + 1u
            addObjectPrefix options newLevel agg pos
            |> joinObject options newLevel props agg
            |> addObjectSuffix options newLevel agg

        and formatString str agg pos =
            insert "\"" agg pos
            |> writeString str agg
            |> insert "\"" agg

        and joinObject options level values agg pos =
            match values with
            | [] -> pos
            | [v] -> formatProperty options level v agg pos
            | v :: vs ->
                joinObject options level vs agg pos
                |> addElementSeparator options level agg
                |> formatProperty options level v agg

        and joinArray options level values agg pos =
            match values with
            | [] -> pos
            | [v] -> formatJson options level v agg pos
            | v :: vs ->
                formatJson options level v agg pos
                |> addElementSeparator options level agg
                |> joinArray options level vs agg

        [<RequireQualifiedAccess>]
        module Json =

            let formatWith options json : string =
                let result = formatJson options 0u json (Array.zeroCreate (calcTokens json |> int)) 0
                System.String.Concat result

            let format json : string =
                formatWith JsonFormattingOptions.Compact json

    module Stream =
        open System.IO
        open System.Text
        open System.Threading.Tasks

        type Formatter<'a> =
            'a -> TextWriter -> TextWriter

        type FormatterTask<'a> =
            'a -> TextWriter -> Task

        module UnitTask =
            let bind (f : Task -> Task) (t:Task) : Task =
                t.ContinueWith(System.Func<Task,Task> f).Unwrap()

        let inline append (s: string) (tw: TextWriter) : TextWriter =
            tw.Write s
            tw

        let inline appendAsTask (s: string) (tw: TextWriter) : Task =
            tw.WriteAsync s

        let rec writeString (cs:char array) (index:int) (tw:TextWriter) : TextWriter =
            if index < (Array.length cs - 1) then
                let nextEscapeIndex = System.Array.FindIndex (cs, index, isEscapeCharPred)
                if nextEscapeIndex = -1 then
                    tw.Write(cs,index,Array.length cs - index)
                    tw
                else if nextEscapeIndex = index then
                    append (escaped cs.[nextEscapeIndex]) tw
                    |> writeString cs (nextEscapeIndex + 1)
                else
                    tw.Write(cs,index,nextEscapeIndex - index)
                    append (escaped cs.[nextEscapeIndex]) tw
                    |> writeString cs (nextEscapeIndex + 1)
            else tw

        let rec writeStringAsTask (cs:char array) (index:int) (tw:TextWriter) : Task =
            if index < (Array.length cs - 1) then
                let nextEscapeIndex = System.Array.FindIndex (cs, index, isEscapeCharPred)
                if nextEscapeIndex = -1 then
                    tw.WriteAsync(cs,index,Array.length cs - index)
                else if nextEscapeIndex = index then
                    appendAsTask (escaped cs.[nextEscapeIndex]) tw
                    |> UnitTask.bind (fun _ -> writeStringAsTask cs (nextEscapeIndex + 1) tw)
                else
                    tw.WriteAsync(cs,index,nextEscapeIndex - index)
                    |> UnitTask.bind (fun _ -> appendAsTask (escaped cs.[nextEscapeIndex]) tw)
                    |> UnitTask.bind (fun _ -> writeStringAsTask cs (nextEscapeIndex + 1) tw)
            else Task.CompletedTask

        let rec join (f: Formatter<'a>) (sep: string) values (tw:TextWriter) : TextWriter =
            match values with
            | [] -> tw
            | [v] -> f v tw
            | v :: vs ->
                join f sep vs tw
                |> append sep
                |> f v

        let rec joinAsTask (f: FormatterTask<'a>) (sep: string) values (tw:TextWriter) : Task =
            match values with
            | [] -> Task.CompletedTask
            | [v] -> f v tw
            | v :: vs ->
                joinAsTask f sep vs tw
                |> UnitTask.bind (fun _ -> appendAsTask sep tw)
                |> UnitTask.bind (fun _ -> f v tw)

        let rec joinRev (f: Formatter<'a>) (sep: string) values (tw:TextWriter) : TextWriter =
            match values with
            | [] -> tw
            | [v] -> f v tw
            | v :: vs ->
                f v tw
                |> append sep
                |> joinRev f sep vs

        let rec joinRevAsTask (f: FormatterTask<'a>) (sep: string) values (tw:TextWriter) : Task =
            match values with
            | [] -> Task.CompletedTask
            | [v] -> f v tw
            | v :: vs ->
                f v tw
                |> UnitTask.bind (fun _ -> appendAsTask sep tw)
                |> UnitTask.bind (fun _ -> joinRevAsTask f sep vs tw)

        let addSeparation { ElementSpacing = es } space tw : TextWriter =
            match es with
            | NoSpaceBetweenElements -> tw
            | SpaceBetweenElements
            | NewLineBetweenElements _ -> append space tw

        let addSeparationAsTask { ElementSpacing = es } space tw : Task =
            match es with
            | NoSpaceBetweenElements -> Task.CompletedTask
            | SpaceBetweenElements
            | NewLineBetweenElements _ -> appendAsTask space tw

        let addPropertyNameSeparator { PropertyNameSpacing = pns } tw : TextWriter =
            match pns with
            | NoSpaceBetweenNameAndValue -> append ":" tw
            | SpaceBetweenNameAndValue -> append ": " tw

        let addPropertyNameSeparatorAsTask { PropertyNameSpacing = pns } tw : Task =
            match pns with
            | NoSpaceBetweenNameAndValue -> appendAsTask ":" tw
            | SpaceBetweenNameAndValue -> appendAsTask ": " tw

        let rec formatJson level options x tw =
            match x with
            | Json.Array elems -> formatArray level options elems tw
            | Json.Bool x -> formatBool x tw
            | Json.Number x -> formatNumber x tw
            | Json.Null -> formatNull tw
            | Json.Object (UnmappedObject o)
            | Json.Object (MappedObject (o,_)) -> formatObject level options o tw
            | Json.String s -> formatString s tw

        and formatArray level options elems tw =
            let newLevel = level + 1u
            let prefix = elementSeparator newLevel options
            let separator = "," + prefix
            let suffix = elementSeparator level options
            append "[" tw
            |> addSeparation options prefix
            |> joinRev (formatJson newLevel options) separator elems
            |> addSeparation options suffix
            |> append "]"

        and formatBool b tw =
            let str =
                if b then "true" else "false"
            append str tw

        and formatNumber n tw =
            append n tw

        and formatNull tw =
            append "null" tw

        and formatProperty level options (k,v) tw =
            formatString k tw
            |> addPropertyNameSeparator options
            |> formatJson level options v

        and formatObject level options props tw =
            let newLevel = level + 1u
            let prefix = elementSeparator newLevel options
            let separator = "," + prefix
            let suffix = elementSeparator level options
            append "{" tw
            |> addSeparation options prefix
            |> join (fun p tw -> formatProperty newLevel options p tw) separator props
            |> addSeparation options suffix
            |> append "}"

        and formatString str tw =
            append "\"" tw
            |> writeString (str.ToCharArray()) 0
            |> append "\""

        let rec formatJsonAsTask level options x tw =
            match x with
            | Json.Array elems -> formatArrayAsTask level options elems tw
            | Json.Bool x -> formatBoolAsTask x tw
            | Json.Number x -> formatNumberAsTask x tw
            | Json.Null -> formatNullAsTask tw
            | Json.Object (UnmappedObject o)
            | Json.Object (MappedObject (o,_)) -> formatObjectAsTask level options o tw
            | Json.String s -> formatStringAsTask s tw

        and formatArrayAsTask level options elems tw =
            let newLevel = level + 1u
            let prefix = elementSeparator newLevel options
            let separator = "," + prefix
            let suffix = elementSeparator level options
            appendAsTask "[" tw
            |> UnitTask.bind (fun _ -> addSeparationAsTask options prefix tw)
            |> UnitTask.bind (fun _ -> joinRevAsTask (formatJsonAsTask newLevel options) separator elems tw)
            |> UnitTask.bind (fun _ -> addSeparationAsTask options suffix tw)
            |> UnitTask.bind (fun _ -> appendAsTask "]" tw)

        and formatBoolAsTask b tw =
            let str =
                if b then "true" else "false"
            appendAsTask str tw

        and formatNumberAsTask n tw =
            appendAsTask n tw

        and formatNullAsTask tw =
            appendAsTask "null" tw

        and formatPropertyAsTask level options (k,v) tw =
            formatStringAsTask k tw
            |> UnitTask.bind (fun _ -> addPropertyNameSeparatorAsTask options tw)
            |> UnitTask.bind (fun _ -> formatJsonAsTask level options v tw)

        and formatObjectAsTask level options props tw =
            let newLevel = level + 1u
            let prefix = elementSeparator newLevel options
            let separator = "," + prefix
            let suffix = elementSeparator level options
            appendAsTask "{" tw
            |> UnitTask.bind (fun _ -> addSeparationAsTask options prefix tw)
            |> UnitTask.bind (fun _ -> joinAsTask (formatPropertyAsTask newLevel options) separator props tw)
            |> UnitTask.bind (fun _ -> addSeparationAsTask options suffix tw)
            |> UnitTask.bind (fun _ -> appendAsTask "}" tw)

        and formatStringAsTask str tw =
            appendAsTask "\"" tw
            |> UnitTask.bind (fun _ -> writeStringAsTask (str.ToCharArray()) 0 tw)
            |> UnitTask.bind (fun _ -> appendAsTask "\"" tw)

        (* Functions *)

        [<RequireQualifiedAccess>]
        module Json =

            let formatInto (tw:#TextWriter) json : unit =
                formatJson 0u JsonFormattingOptions.Compact json tw
                |> ignore

            let formatIntoWith options (tw:#TextWriter) json : unit =
                formatJson 0u options json tw
                |> ignore

            let formatIntoAsTask (tw:TextWriter) json : Task =
                formatJsonAsTask 0u JsonFormattingOptions.Compact json tw

            let formatIntoWithAsTask options (tw:#TextWriter) json : Task =
                formatJsonAsTask 0u options json tw

            let formatIntoAsAsync (tw:#TextWriter) json : Async<unit> =
                async.Delay(fun () -> (formatIntoAsTask tw json).ContinueWith(fun _ -> ()) |> Async.AwaitTask)

            let formatIntoWithAsAsync options (tw:#TextWriter) json : Async<unit> =
                async.Delay(fun () -> (formatIntoWithAsTask options tw json).ContinueWith(fun _ -> ()) |> Async.AwaitTask)


//     (* Functions *)

//     [<RequireQualifiedAccess>]
//     module Json =

//         let format json =
//             StringBuilder ()
//             |> formatJson 0 JsonFormattingOptions.Compact json
//             |> string

//         let formatWith options json =
//             StringBuilder ()
//             |> formatJson 0 options json
//             |> string

//     (* Error Message Formatters *)

//     [<RequireQualifiedAccess>]
//     module Errors =

//         let missingMember key =
//             sprintf "Error deserializing JSON object; Missing required member '%s'" key

//         let missingMemberWithJson key =
//             function | Some format -> Json.formatWith format >> (+) (missingMember key + ": ")
//                      | None -> fun _ -> missingMember key

// (* Mapping

//    Functional mapping between Json and native F# data structures,
//    through statically inferred types. Types providing FromJson and
//    ToJson static members with appropriate signatures can be
//    seamlessly serialized and deserialized.

//    This approach is the same as that taken by the Fleece library,
//    credit for which is due to Mauricio Scheffer. *)

// [<AutoOpen>]
// module Mapping =

//     open Operators

//     (* From

//        Default conversion functions (static members on FromJsonDefaults)
//        and statically inferred inline conversion functions for conversion
//        from Json to F# data structures. *)

//     (* Defaults *)

//     type FromJsonDefaults = FromJsonDefaults with

//         (* Basic Types *)

//         static member inline FromJson (_: unit) =
//             Json.Optic.get Json.Null_

//         static member inline FromJson (_: bool) =
//             Json.Optic.get Json.Bool_

//         static member inline FromJson (_: decimal) =
//             id <!> Json.Optic.get Json.Number_

//         static member inline FromJson (_: float) =
//             float <!> Json.Optic.get Json.Number_

//         static member inline FromJson (_: int) =
//             int <!> Json.Optic.get Json.Number_

//         static member inline FromJson (_: int16) =
//             int16 <!> Json.Optic.get Json.Number_

//         static member inline FromJson (_: int64) =
//             int64 <!> Json.Optic.get Json.Number_

//         static member inline FromJson (_: single) =
//             single <!> Json.Optic.get Json.Number_

//         static member inline FromJson (_: string) =
//             Json.Optic.get Json.String_

//         static member inline FromJson (_: uint16) =
//             uint16 <!> Json.Optic.get Json.Number_

//         static member inline FromJson (_: uint32) =
//             uint32 <!> Json.Optic.get Json.Number_

//         static member inline FromJson (_: uint64) =
//             uint64 <!> Json.Optic.get Json.Number_

//         (* Common Types *)

//         static member inline FromJson (_: DateTime) =
//                 fun x ->
//                     match DateTime.TryParseExact (x, [| "s"; "r"; "o" |], null, DateTimeStyles.AdjustToUniversal) with
//                     | true, x -> Json.init x
//                     | _ -> Json.error "datetime"
//             =<< Json.Optic.get Json.String_

//         static member inline FromJson (_: DateTimeOffset) =
//                 fun x ->
//                     match DateTimeOffset.TryParseExact (x, [| "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF'Z'"; "o"; "r" |], null, DateTimeStyles.AssumeUniversal) with
//                     | true, x -> Json.init x
//                     | _ -> Json.error "datetimeoffset"
//             =<< Json.Optic.get Json.String_

//         static member inline FromJson (_: Guid) =
//                 fun x ->
//                     match Guid.TryParse x with
//                     | true, x -> Json.init x
//                     | _ -> Json.error "guid"
//             =<< Json.Optic.get Json.String_

//         (* Json Type *)

//         static member inline FromJson (_: Json) =
//             Json.Optic.get id_

//     (* Mapping Functions

//        Functions for applying the FromJson function to Json to produce
//        new instances of 'a where possible, including folding the FromJson
//        function across a list of Json objects. *)

//     let inline internal fromJsonDefaults (a: ^a, _: ^b) =
//         ((^a or ^b) : (static member FromJson: ^a -> ^a Json) a)

//     let inline internal fromJson x =
//         fst (fromJsonDefaults (Unchecked.defaultof<'a>, FromJsonDefaults) x)

//     let inline internal fromJsonFold init fold xs =
//         List.fold (fun r x ->
//             match r with
//             | Error e ->
//                 Error e
//             | Value xs ->
//                 match fromJson x with
//                 | Value x -> Value (fold x xs)
//                 | Error e -> Error e) (Value init) (List.rev xs)

//     (* Defaults *)

//     type FromJsonDefaults with

//         (* Arrays *)

//         static member inline FromJson (_: 'a array) : Json<'a array> =
//                 fromJsonFold [||] (fun x xs -> Array.append [| x |] xs) >> Json.ofResult
//             =<< Json.Optic.get Json.Array_

//         (* Lists *)

//         static member inline FromJson (_: 'a list) : Json<'a list> =
//                 fromJsonFold [] (fun x xs -> x :: xs) >> Json.ofResult
//             =<< Json.Optic.get Json.Array_

//         (* Maps *)

//         static member inline FromJson (_: Map<string,'a>) : Json<Map<string,'a>> =
//                 fun x ->
//                     let k, v = (Map.toList >> List.unzip) x
//                     List.zip k >> Map.ofList <!> Json.ofResult (fromJsonFold [] (fun x xs -> x :: xs) v)
//             =<< Json.Optic.get Json.Object_

//         (* Sets *)

//         static member inline FromJson (_: Set<'a>) : Json<Set<'a>> =
//                 fromJsonFold Set.empty Set.add >> Json.ofResult
//             =<< Json.Optic.get Json.Array_

//         (* Options *)

//         static member inline FromJson (_: 'a option) : Json<'a option> =
//                 function | Null _ -> Json.init None
//                          | x -> Some <!> Json.ofResult (fromJson x)
//             =<< Json.Optic.get id_

//         (* Tuples *)

//         static member inline FromJson (_: 'a * 'b) : Json<'a * 'b> =
//                 function | a :: [b] ->
//                                 fun a b -> a, b
//                             <!> Json.ofResult (fromJson a)
//                             <*> Json.ofResult (fromJson b)
//                          | _ ->
//                             Json.error "tuple2"
//             =<< Json.Optic.get Json.Array_

//         static member inline FromJson (_: 'a * 'b * 'c) : Json<'a * 'b * 'c> =
//                 function | a :: b :: [c] ->
//                                 fun a b c -> a, b, c
//                             <!> Json.ofResult (fromJson a)
//                             <*> Json.ofResult (fromJson b)
//                             <*> Json.ofResult (fromJson c)
//                          | _ ->
//                             Json.error "tuple3"
//             =<< Json.Optic.get Json.Array_

//         static member inline FromJson (_: 'a * 'b * 'c * 'd) : Json<'a * 'b * 'c * 'd> =
//                 function | a :: b :: c :: [d] ->
//                                 fun a b c d -> a, b, c, d
//                             <!> Json.ofResult (fromJson a)
//                             <*> Json.ofResult (fromJson b)
//                             <*> Json.ofResult (fromJson c)
//                             <*> Json.ofResult (fromJson d)
//                          | _ ->
//                             Json.error "tuple4"
//             =<< Json.Optic.get Json.Array_

//         static member inline FromJson (_: 'a * 'b * 'c * 'd * 'e) : Json<'a * 'b * 'c * 'd * 'e> =
//                 function | a :: b :: c :: d :: [e] ->
//                                 fun a b c d e -> a, b, c, d, e
//                             <!> Json.ofResult (fromJson a)
//                             <*> Json.ofResult (fromJson b)
//                             <*> Json.ofResult (fromJson c)
//                             <*> Json.ofResult (fromJson d)
//                             <*> Json.ofResult (fromJson e)
//                          | _ ->
//                             Json.error "tuple5"
//             =<< Json.Optic.get Json.Array_

//     (* To

//         *)

//     (* Defaults *)

//     type ToJsonDefaults = ToJsonDefaults with

//         (* Basic Types *)

//         static member inline ToJson (x: unit) =
//             Json.Optic.set Json.Null_ x

//         static member inline ToJson (x: bool) =
//             Json.Optic.set Json.Bool_ x

//         static member inline ToJson (x: decimal) =
//             Json.Optic.set Json.Number_ x

//         static member inline ToJson (x: float) =
//             match x with
//             | x when Double.IsInfinity x -> failwith "Serialization of Infinite Numbers Invalid."
//             | x when Double.IsNaN x -> failwith "Serialization of NaN Invalid."
//             | x -> Json.Optic.set Json.Number_ (decimal x)

//         static member inline ToJson (x: int) =
//             Json.Optic.set Json.Number_ (decimal x)

//         static member inline ToJson (x: int16) =
//             Json.Optic.set Json.Number_ (decimal x)

//         static member inline ToJson (x: int64) =
//             Json.Optic.set Json.Number_ (decimal x)

//         static member inline ToJson (x: single) =
//             match x with
//             | x when Single.IsInfinity x -> failwith "Serialization of Infinite Numbers Invalid."
//             | x when Single.IsNaN x -> failwith "Serialization of NaN Invalid."
//             | x -> Json.Optic.set Json.Number_ (decimal x)

//         static member inline ToJson (x: string) =
//             Json.Optic.set Json.String_ x

//         static member inline ToJson (x: uint16) =
//             Json.Optic.set Json.Number_ (decimal x)

//         static member inline ToJson (x: uint32) =
//             Json.Optic.set Json.Number_ (decimal x)

//         static member inline ToJson (x: uint64) =
//             Json.Optic.set Json.Number_ (decimal x)

//         (* Common Types *)

//         static member inline ToJson (x: DateTime) =
//             Json.Optic.set Json.String_ (x.ToUniversalTime().ToString("o"))

//         static member inline ToJson (x: DateTimeOffset) =
//             Json.Optic.set Json.String_ (x.ToString("o"))

//         static member inline ToJson (x: Guid) =
//             Json.Optic.set Json.String_ (string x)

//         (* Json Type *)

//         static member inline ToJson (x: Json) =
//             Json.Optic.set id_ x

//     (* Mapping Functions

//        Functions for applying the ToJson function to data structures to produce
//        new Json instances. *)

//     let inline internal toJsonDefaults (a: ^a, _: ^b) =
//         ((^a or ^b) : (static member ToJson: ^a -> unit Json) a)

//     let inline internal toJson (x: 'a) =
//         snd (toJsonDefaults (x, ToJsonDefaults) (Object (Map.empty)))

//     let inline internal toJsonWith (f:'a -> unit Json) (x: 'a) =
//         snd (f x (Object (Map.empty)))

//     (* Defaults *)

//     type ToJsonDefaults with

//         (* Arrays *)

//         static member inline ToJson (x: 'a array) =
//             Json.Optic.set id_ (Array ((Array.toList >> List.map toJson) x))

//         (* Lists *)

//         static member inline ToJson (x: 'a list) =
//             Json.Optic.set id_ (Array (List.map toJson x))

//         (* Maps *)

//         static member inline ToJson (x: Map<string,'a>) =
//             Json.Optic.set id_ (Object (Map.map (fun _ a -> toJson a) x))

//         (* Options *)

//         static member inline ToJson (x: 'a option) =
//             Json.Optic.set id_ ((function | Some a -> toJson a
//                                           | _ -> Null ()) x)

//         (* Sets *)

//         static member inline ToJson (x: Set<'a>) =
//             Json.Optic.set id_ (Array ((Set.toList >> List.map toJson) x))

//         (* Tuples *)

//         static member inline ToJson ((a, b)) =
//             Json.Optic.set id_ (Array [ toJson a; toJson b ])

//         static member inline ToJson ((a, b, c)) =
//             Json.Optic.set id_ (Array [ toJson a; toJson b; toJson c ])

//         static member inline ToJson ((a, b, c, d)) =
//             Json.Optic.set id_ (Array [ toJson a; toJson b; toJson c; toJson d ])

//         static member inline ToJson ((a, b, c, d, e)) =
//             Json.Optic.set id_ (Array [ toJson a; toJson b; toJson c; toJson d; toJson e ])

//     (* Functions

//         *)

//     [<RequireQualifiedAccess>]
//     module Json =

//         (* Read/Write *)

//         let missingMember key =
//             fun json ->
//                 Errors.missingMemberWithJson key (Some JsonFormattingOptions.SingleLine) json
//                 |> fun e -> Error e, json

//         let readMemberWith fromJson key onMissing =
//                 Json.Optic.tryGet (Json.Object_ >?> Map.key_ key)
//             >>= function | Some json -> Json.ofResult (fromJson json)
//                          | None -> onMissing ()

//         let inline readWith fromJson key =
//             readMemberWith fromJson key <| fun () -> missingMember key

//         let inline read key =
//             readWith fromJson key

//         let inline readWithOrDefault fromJson key def =
//             readMemberWith fromJson key <| fun () -> Json.init def

//         let inline readOrDefault key def =
//             readWithOrDefault fromJson key def

//         let inline tryReadWith fromJson key =
//             readMemberWith fromJson key <| fun () -> Json.init None

//         let inline tryRead key =
//             tryReadWith fromJson key

//         let writeWith toJson key value =
//             Json.Optic.set (Json.Object_ >?> Map.value_ key) (Some (toJson value))

//         let inline write key value =
//             writeWith toJson key value

//         let writeWithUnlessDefault toJson key def value =
//             match value with
//             | v when v = def -> Json.ofResult <| Value ()
//             | _ -> writeWith toJson key value

//         let inline writeUnlessDefault key def value =
//             writeWithUnlessDefault toJson key def value

//         let writeNone key =
//             Json.Optic.set (Json.Object_ >?> Map.value_ key) (Some (Json.Null ()))

//         (* Serialization/Deserialization *)

//         let inline deserialize json =
//             fromJson json
//             |> function | Value a -> a
//                         | Error e -> failwith e

//         let inline tryDeserialize json =
//             fromJson json
//             |> function | Value a -> Choice1Of2 a
//                         | Error e -> Choice2Of2 e

//         let inline serialize a =
//             toJson a

//         let inline serializeWith f a =
//             toJsonWith f a

// (* Patterns

//    Active patterns for working with Json data structures, making it
//    easier to write code for matching against unions, etc. *)

// [<AutoOpen>]
// module Patterns =

//     open Aether.Operators

//     /// Parse a Property from a Json Object token using a supplied fromJson,
//     /// and try to deserialize it to the inferred type.
//     let inline (|PropertyWith|) fromJson key =
//             Optic.get (Json.Object_ >?> Map.key_ key)
//          >> Option.bind (fromJson >> function | Value a, _ -> Some a
//                                               | _ -> None)

//     /// Parse a Property from a Json Object token, and try to deserialize it to the
//     /// inferred type.
//     let inline (|Property|_|) key =
//             Optic.get (Json.Object_ >?> Map.key_ key)
//          >> Option.bind (Json.tryDeserialize >> function | Choice1Of2 a -> Some a
//                                                          | _ -> None)

[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo ("Chiron.Tests")>]
()
