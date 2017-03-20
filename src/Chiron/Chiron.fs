namespace Chiron

type JsonMemberType =
    | Object
    | Array
    | String
    | Number
    | Bool
    | Null

type [<StructuralEquality;NoComparison>] Json =
    | Object of properties: JsonObject
    | Array of elements: Json list
    | String of string
    | Number of number: string
    | True
    | False
    | Null

and [<CustomEquality;NoComparison>] JsonObject =
    | WriteObject of propList: (string * Json) list
    | ReadObject of propList: (string * Json) list * propMap: Map<string,Json>
    override x.Equals(o) =
        match o with
        | null -> false
        | :? JsonObject as y->
            match x, y with
            | WriteObject xps, WriteObject yps -> Map.ofList (List.rev xps) = Map.ofList (List.rev yps)
            | ReadObject (_, mps), WriteObject ps -> Map.ofList (List.rev ps) = mps
            | WriteObject ps, ReadObject (_, mps) -> Map.ofList (List.rev ps) = mps
            | ReadObject (_, xps), ReadObject (_, yps) -> xps = yps
        | _ -> false
    override x.GetHashCode() =
        match x with
        | WriteObject ps -> Map.ofList (List.rev ps) |> hash
        | ReadObject (_, mps) -> mps |> hash

type JsonFailure =
    | Tagged of propertyName: string * failure: JsonFailure
    | NoInput
    | PropertyNotFound
    | TypeMismatch of expected: JsonMemberType * actual: JsonMemberType
    | DeserializationError of targetType: System.Type * err: string
    | ParserFailure of parserFail: string

type JsonResult<'a> = Result<'a,JsonFailure list>
type Json<'a> = Json -> JsonResult<'a> * Json
type JsonReader<'a> = Json -> JsonResult<'a>
type ObjectReader<'a> = JsonObject -> JsonResult<'a>
type JsonWriter<'a> = 'a -> Json
type ObjectWriter<'a> = 'a -> JsonObject -> JsonObject

type ChironDefaults = ChironDefaults

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonMemberType =
    let ofJson = function
        | Json.Object _ -> JsonMemberType.Object
        | Json.Array _ -> JsonMemberType.Array
        | Json.String _ -> JsonMemberType.String
        | Json.Number _ -> JsonMemberType.Number
        | Json.True -> JsonMemberType.Bool
        | Json.False -> JsonMemberType.Bool
        | Json.Null -> JsonMemberType.Null

    let describe = function
        | JsonMemberType.Object -> "an object"
        | JsonMemberType.Array -> "an array"
        | JsonMemberType.String -> "a string"
        | JsonMemberType.Number -> "a number"
        | JsonMemberType.Bool -> "a boolean"
        | JsonMemberType.Null -> "null"

module Optic =
    type Epi<'a,'b> = ('a -> JsonResult<'b>) * ('b -> 'a)
    type Lens<'a,'b> = ('a -> JsonResult<'b>) * ('b -> 'a -> 'a)

    let get (l:Lens<'a,'b>) =
        fst l

    let set (l:Lens<'a,'b>) =
        snd l

    let compose (l1:Lens<'a,'b>) (l2:Lens<'b,'c>): Lens<'a,'c> =
        (fun a -> fst l1 a |> Result.bind (fst l2)), (fun c a -> match Result.map (snd l2 c) (fst l1 a) with Ok b -> snd l1 b a; | _ -> a)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonFailure =
    let rec toString = function
        | Tagged (p, (Tagged _ as f)) -> System.String.Concat (p, ".", toString f)
        | Tagged (p,f) -> System.String.Concat (p, ": ", toString f)
        | NoInput -> "No input was provided"
        | PropertyNotFound -> "Failed to find expected property"
        | TypeMismatch (e,a) -> System.String.Concat ("Expected to find ", JsonMemberType.describe e, ", but instead found ", JsonMemberType.describe a)
        | DeserializationError (t,e) -> System.String.Concat ("Unable to deserialize value as '", t.FullName, "': ", e)
        | ParserFailure e -> "Invalid JSON, failed to parse: " + e

    let withTag tag (a2bR : 'a -> Result<'b,JsonFailure list>) (a : 'a) =
        match a2bR a with
        | Ok a -> Ok a
        | Error errs -> List.map (fun e -> Tagged (tag, e)) errs |> Error

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonObject =
    let empty = WriteObject []

    let propertyFolder (toJson: JsonWriter<'a>) s (k,v) = (k, toJson v) :: s

    let ofPropertyListWith (toJson: JsonWriter<'a>) (ps: (string * 'a) list): JsonObject =
        List.fold (propertyFolder toJson) [] ps
        |> JsonObject.WriteObject

    let toPropertyList = function
        | WriteObject ps -> ps
        | ReadObject (ps, _) -> ps

    let toJson jObj = Json.Object jObj

    let ofPropertyList (ps: (string * Json) list): JsonObject =
        List.rev ps
        |> JsonObject.WriteObject

    let ofMap m = ReadObject (Map.toList m, m)

    let ofMapWith toJson m = let newMap = Map.map (fun _ -> toJson) m in ReadObject (Map.toList newMap |> List.rev, newMap)

    let add k v = function
        | WriteObject ps -> WriteObject ((k,v) :: ps)
        | ReadObject (ps, mps) ->
            let newMps = Map.add k v mps
            ReadObject ((k,v) :: ps, newMps)

    let remove k = function
        | WriteObject ps -> WriteObject (List.filter (fun (k',_) -> k' <> k) ps)
        | ReadObject (ps, mps) -> ReadObject (List.filter (fun (k',_) -> k' <> k) ps, Map.remove k mps)

    let tryGet k = function
        | WriteObject ps -> List.tryPick (fun (k',v) -> if k' = k then Some v else None) ps
        | ReadObject (_, mps) -> Map.tryFind k mps

    let handleTryFind k = function
        | Some v -> Ok v
        | None -> Error [Tagged (k, PropertyNotFound)]

    let tryGetOrFail k jsonObj =
        tryGet k jsonObj
        |> handleTryFind k

    let writeWith (serialize : JsonWriter<'a>) (k : string) (v : 'a) (jsonObject : JsonObject) =
        add k (serialize v) jsonObject

    let inline writeWithDefault (defaults : ^def) (k : string) (v : ^a) (jsonObject : JsonObject) =
        add k ((^a or ^def) : (static member ToJson : ^a -> Json) v) jsonObject

    let inline write k v (jsonObject : JsonObject) =
        writeWithDefault ChironDefaults k v jsonObject

    let writeOptWith (serialize : JsonWriter<'a>) (k : string) (vO : 'a option) (jsonObject : JsonObject) =
        match vO with
        | None -> jsonObject
        | Some v -> add k (serialize v) jsonObject

    let inline writeOptWithDefault (defaults : ^def) (k : string) (vO : ^a option) (jsonObject : JsonObject) =
        match vO with
        | None -> jsonObject
        | Some v -> add k ((^a or ^def) : (static member ToJson : ^a -> Json) v) jsonObject

    let inline writeOpt k vO (jsonObject : JsonObject) =
        writeOptWithDefault ChironDefaults k vO jsonObject

    let writeObjWith (writer: ObjectWriter<'a>) (k: string) (v: 'a) (jsonObject: JsonObject) : JsonObject =
        let json = writer v empty |> toJson
        add k json jsonObject

    let inline writeObjWithDefault (defaults : ^def) (k : string) (v : ^a) (jsonObject : JsonObject) =
        let json = ((^a or ^def) : (static member Encode : ^a -> JsonObject -> JsonObject) (v, empty)) |> toJson
        add k json jsonObject

    let inline writeObj (k : string) (v : ^a) (jsonObject : JsonObject) =
        writeObjWithDefault ChironDefaults k v jsonObject

    let writeObjOptWith (writer: ObjectWriter<'a>) (k: string) (vO: 'a option) (jsonObject: JsonObject) : JsonObject =
        match vO with
        | None -> jsonObject
        | Some v ->
            writeObjWith writer k v jsonObject

    let inline writeObjOptWithDefault (defaults : ^def) (k : string) (vO : ^a option) (jsonObject : JsonObject) =
        match vO with
        | None -> jsonObject
        | Some v ->
            writeObjWithDefault defaults k v jsonObject

    let inline writeObjOpt (k : string) (vO : ^a option) (jsonObject : JsonObject) =
        writeObjOptWithDefault ChironDefaults k vO jsonObject

    let inline mixinObj (writer: ObjectWriter<'a>) v jObj =
        writer v jObj

    let mixinObjOpt writer vO jObj =
        match vO with
        | None -> jObj
        | Some v -> mixinObj writer v jObj

    let build (writer: ObjectWriter<'a>) v =
        writer v empty
        |> Json.Object

    let readWith (deserialize : JsonReader<'a>) (k: string) (jsonObject: JsonObject) : Result<'a,JsonFailure list> =
        tryGetOrFail k jsonObject
        |> Result.bind (JsonFailure.withTag k deserialize)

    let inline readWithDefault (defaults : ^def) (k : string) (jsonObject: JsonObject) : Result<'a,JsonFailure list> =
        let deserializer= ((^a or ^def) : (static member FromJson : ^a -> JsonReader<'a>) Unchecked.defaultof<'a>)
        readWith deserializer k jsonObject

    let inline read (k: string) (jsonObject: JsonObject) : Result<'a,JsonFailure list> =
        readWithDefault ChironDefaults k jsonObject

    let readOptWith (deserialize : JsonReader<'a>) (k: string) (jsonObject: JsonObject) : Result<'a option,JsonFailure list> =
        tryGet k jsonObject
        |> Option.map (JsonFailure.withTag k deserialize)
        |> function
            | Some (Ok a) -> Ok (Some a)
            | Some (Error e) -> Error e
            | None -> Ok None

    let inline readOptWithDefault (defaults : ^def) (k : string) (jsonObject: JsonObject) : Result<'a option,JsonFailure list> =
        let deserializer = ((^a or ^def) : (static member FromJson : ^a -> JsonReader<'a>) Unchecked.defaultof<'a>)
        readOptWith deserializer k jsonObject

    let inline readOpt (k: string) (jsonObject: JsonObject) : Result<'a option,JsonFailure list> =
        readOptWithDefault ChironDefaults k jsonObject

    let optimizeRead = function
        | WriteObject ps -> ReadObject (ps, Map.ofList (List.rev ps))
        | o -> o

    let optimizeAppend = function
        | ReadObject (ps, mps) -> WriteObject ps
        | o -> o

    let dedupeWithMap kvps m =
        if List.length kvps = Map.count m then
            kvps
        else
            let hs = System.Collections.Generic.HashSet ()
            List.foldBack (fun (k,_) s -> if hs.Add(k) then (k, Map.find k m) :: s else s) kvps []

    let dedupeList kvps =
        dedupeWithMap kvps (Map.ofList (List.rev kvps))

    let optimizeWrite = function
        | WriteObject ps -> WriteObject (dedupeList ps)
        | ReadObject (ps, mps) -> ReadObject (dedupeWithMap ps mps, mps)

    module Optics =
        let key_ k =
            (function
                | JsonObject.ReadObject (_, mps) -> Map.tryFind k mps |> handleTryFind k
                | JsonObject.WriteObject ps -> List.tryPick (fun (k',v) -> if k' = k then Some v else None) ps |> handleTryFind k),
            (fun v -> function
                | JsonObject.WriteObject ps -> JsonObject.WriteObject ((k,v) :: ps)
                | JsonObject.ReadObject (ps, mps) ->
                    let newMps = Map.add k v mps
                    JsonObject.ReadObject ((k,v) :: ps, newMps))

module JsonResult =
    let withThrowingParser (parser : string -> 'a) (str : string): JsonResult<'a> =
        try
            parser str
            |> Ok
        with
        | e -> Error [DeserializationError (typeof<'a>, e.Message)]

    let deserializationError<'a> (err: string) : JsonResult<'a> =
        Error [DeserializationError (typeof<'a>, err)]

    let summarize = function
        | Ok x -> sprintf "No errors"
        | Error [e] -> sprintf "Found 1 error:\n  %s" <| JsonFailure.toString e
        | Error errs ->
            let sb = System.Text.StringBuilder()
            let sb = sb.AppendLine(sprintf "Found %i errors:" (List.length errs))
            let sb = errs |> List.fold (fun (sb:System.Text.StringBuilder) e -> sb.Append("  ").AppendLine(JsonFailure.toString e)) sb
            sb.ToString()

    let apply (aR: Result<'a,'e>) (a2Rb: Result<'a -> 'b,'e>) : Result<'b,'e> =
        match a2Rb with
        | Ok a2b ->
            match aR with
            | Ok a -> Ok (a2b a)
            | Error e -> Error e
        | Error e -> Error e

    let applyWith (c2aR: 'c -> Result<'a,'e>) (c: 'c) (a2Rb: Result<'a -> 'b,'e>) : Result<'b,'e> =
        match a2Rb with
        | Ok a2b ->
            match c2aR c with
            | Ok a -> Ok (a2b a)
            | Error e -> Error e
        | Error e -> Error e

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Json =
    let ofPropertyList (ps: (string * Json) list): Json =
        JsonObject.ofPropertyList ps
        |> JsonObject.toJson

    let ofPropertyListWith (toJson: JsonWriter<'a>) (ps: (string * 'a) list): Json =
        JsonObject.ofPropertyListWith toJson ps
        |> JsonObject.toJson

    let ofJsonObject (jObj: JsonObject): Json =
        Json.Object jObj

    let ofListWith (toJson: JsonWriter<'a>) (els: 'a list): Json =
        List.map toJson els
        |> Json.Array

    let ofList (els: Json list): Json =
        Json.Array els

    let ofString (str: string): Json =
        Json.String str

    let ofInt16 (n: int16): Json =
        Json.Number (string n)

    let ofInt32 (n: int): Json =
        Json.Number (string n)

    let ofInt64 (n: int64): Json =
        Json.Number (string n)

    let ofUInt16 (n: uint16): Json =
        Json.Number (string n)

    let ofUInt32 (n: uint32): Json =
        Json.Number (string n)

    let ofUInt64 (n: uint64): Json =
        Json.Number (string n)

    let ofSingle (n: single): Json =
        Json.Number (n.ToString("R"))

    let ofFloat (n: float): Json =
        Json.Number (n.ToString("G17"))

    let ofDecimal (n: decimal): Json =
        Json.Number (string n)

    let ofBigInt (n: bigint): Json =
        Json.Number (n.ToString("R"))

    let ofDateTime (dt: System.DateTime): Json =
        Json.String (dt.ToUniversalTime().ToString("o"))

    let ofDateTimeOffset (dt: System.DateTimeOffset): Json =
        Json.String (dt.ToString("o"))

    let ofGuid (g: System.Guid): Json =
        Json.String (string g)

    let ofBytes (bs: byte array): Json =
        System.Convert.ToBase64String bs
        |> Json.String

    let ofBool (b: bool): Json =
        if b then Json.True else Json.False

    let ``null``: Json = Json.Null

    module Inference =
        type ToJsonDefaults = ToJsonDefaults with
            static member ToJson (ps: (string * Json) list): Json = ofPropertyList ps
            static member ToJson (jObj: JsonObject): Json = ofJsonObject jObj
            static member ToJson (els: Json list): Json = ofList els
            static member ToJson (str: string): Json = ofString str
            static member ToJson (n: int16): Json = ofInt16 n
            static member ToJson (n: int): Json = ofInt32 n
            static member ToJson (n: int64): Json = ofInt64 n
            static member ToJson (n: uint16): Json = ofUInt16 n
            static member ToJson (n: uint32): Json = ofUInt32 n
            static member ToJson (n: uint64): Json = ofUInt64 n
            static member ToJson (n: single): Json = ofSingle n
            static member ToJson (n: float): Json = ofFloat n
            static member ToJson (n: decimal): Json = ofDecimal n
            static member ToJson (n: bigint): Json = ofBigInt n
            static member ToJson (b: bool): Json = ofBool b
            static member ToJson (dt: System.DateTime): Json = ofDateTime dt
            static member ToJson (dt: System.DateTimeOffset): Json = ofDateTimeOffset dt
            static member ToJson (g: System.Guid): Json = ofGuid g
            static member ToJson (bs: byte array): Json = ofBytes bs
            static member ToJson (): Json = ``null``

    let inline inferWithDefaults (defaults: ^def) (a: ^a): Json =
        ((^a or ^def) : (static member ToJson : ^a -> Json) a)

    let inline infer (a: 'a) =
        inferWithDefaults Inference.ToJsonDefaults a

    let init (a: 'a) : Json<'a> =
        fun json ->
            (Ok a, json)

    let error (e: JsonFailure) : Json<'a> =
        fun json ->
            (Error [e], json)

    let ofResult result : Json<'a> =
        fun json ->
            (result, json)

    let bind (a2bJ: 'a -> Json<'b>) (aJ: Json<'a>) : Json<'b> =
        fun json ->
            match aJ json with
            | Ok a, json' -> a2bJ a json'
            | Error e, json' -> Error e, json'

    let apply (aJ: Json<'a>) (a2Jb: Json<'a -> 'b>) : Json<'b> =
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
        map a2b2c aJ
        |> apply bJ

    let map3 (a2b2c2d: 'a -> 'b -> 'c -> 'd) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) : Json<'d> =
        map a2b2c2d aJ
        |> apply bJ
        |> apply cJ

    let map4 (a2b2c2d2x: 'a -> 'b -> 'c -> 'd -> 'x) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) : Json<'x> =
        map a2b2c2d2x aJ
        |> apply bJ
        |> apply cJ
        |> apply dJ

    let map5 (a2b2c2d2x2y: 'a -> 'b -> 'c -> 'd -> 'x -> 'y) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) (xJ: Json<'x>) : Json<'y> =
        map a2b2c2d2x2y aJ
        |> apply bJ
        |> apply cJ
        |> apply dJ
        |> apply xJ

    let map6 (a2b2c2d2x2y2z: 'a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) (xJ: Json<'x>) (yJ: Json<'y>) : Json<'z> =
        map a2b2c2d2x2y2z aJ
        |> apply bJ
        |> apply cJ
        |> apply dJ
        |> apply xJ
        |> apply yJ

    module Optics =
        let Object__ =
            (function
                | Json.Object o -> Ok o
                | x -> Error [TypeMismatch (JsonMemberType.Object, JsonMemberType.ofJson x)]),
            (Json.Object)

        let Array__ =
            (function
                | Json.Array a -> Ok a
                | x -> Error [TypeMismatch (JsonMemberType.Array, JsonMemberType.ofJson x)]),
            (Json.Array)

        let String__ =
            (function
                | Json.String s -> Ok s
                | x -> Error [TypeMismatch (JsonMemberType.String, JsonMemberType.ofJson x)]),
            (Json.String)

        let Number__ =
            (function
                | Json.Number n -> Ok n
                | x -> Error [TypeMismatch (JsonMemberType.Number, JsonMemberType.ofJson x)]),
            (Json.Number)

        let Bool__ =
            (function
                | Json.True -> Ok true
                | Json.False -> Ok false
                | x -> Error [TypeMismatch (JsonMemberType.Bool, JsonMemberType.ofJson x)]),
            (fun b -> if b then Json.True else Json.False)

        let Null__ =
            (function
                | Json.Null -> Ok ()
                | x -> Error [TypeMismatch (JsonMemberType.Null, JsonMemberType.ofJson x)]),
            (fun () -> Json.Null)

        let Object_ =
            fst Object__, fun o _ -> Json.Object o

        let Array_ : Optic.Lens<Json,Json list> =
            fst Array__, fun a _ -> Json.Array a

        let String_ =
            fst String__, fun s _ -> Json.String s

        let Number_ : Optic.Lens<Json,string> =
            fst Number__, fun n _ -> Json.Number n

        let Bool_ =
            fst Bool__, fun b _ -> if b then Json.True else Json.False

        let Null_ =
            fst Null__, fun () _ -> Json.Null

        let makeLens (outer: Optic.Lens<Json,'a>) (get: 'a -> JsonResult<'b>) (toJson: 'b -> Json) : Optic.Lens<Json,'b> =
            (fst outer >> Result.bind get, fun b _ -> toJson b)

        let Int16_ : Optic.Lens<Json,int16> =
            makeLens Number_ (JsonResult.withThrowingParser System.Int16.Parse) ofInt16

        let Int32_ : Optic.Lens<Json,int> =
            makeLens Number_ (JsonResult.withThrowingParser System.Int32.Parse) ofInt32

        let Int64_ : Optic.Lens<Json,int64> =
            makeLens Number_ (JsonResult.withThrowingParser System.Int64.Parse) ofInt64

        let UInt16_ : Optic.Lens<Json,uint16> =
            makeLens Number_ (JsonResult.withThrowingParser System.UInt16.Parse) ofUInt16

        let UInt32_ : Optic.Lens<Json,uint32> =
            makeLens Number_ (JsonResult.withThrowingParser System.UInt32.Parse) ofUInt32

        let UInt64_ : Optic.Lens<Json,uint64> =
            makeLens Number_ (JsonResult.withThrowingParser System.UInt64.Parse) ofUInt64

        let Single_ : Optic.Lens<Json,single> =
            makeLens Number_ (JsonResult.withThrowingParser System.Single.Parse) ofSingle

        let Float_ : Optic.Lens<Json,float> =
            makeLens Number_ (JsonResult.withThrowingParser System.Double.Parse) ofFloat

        let Decimal_ : Optic.Lens<Json,decimal> =
            makeLens Number_ (JsonResult.withThrowingParser System.Decimal.Parse) ofDecimal

        let BigInt_ : Optic.Lens<Json,bigint> =
            makeLens Number_ (JsonResult.withThrowingParser System.Numerics.BigInteger.Parse) ofBigInt

        let dateTimeParser s = System.DateTime.ParseExact (s, [| "s"; "r"; "o" |], null, System.Globalization.DateTimeStyles.AdjustToUniversal)
        let DateTime_ : Optic.Lens<Json,System.DateTime> =
            makeLens String_ (JsonResult.withThrowingParser dateTimeParser) ofDateTime

        let dateTimeOffsetParser s = System.DateTimeOffset.ParseExact (s, [| "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF'Z'"; "o"; "r" |], null, System.Globalization.DateTimeStyles.AssumeUniversal)
        let DateTimeOffset_ : Optic.Lens<Json,System.DateTimeOffset> =
            makeLens String_ (JsonResult.withThrowingParser dateTimeOffsetParser) ofDateTimeOffset

        let Guid_ : Optic.Lens<Json,System.Guid> =
            makeLens String_ (JsonResult.withThrowingParser System.Guid.Parse) ofGuid

        let Bytes_ : Optic.Lens<Json,byte array> =
            makeLens String_ (JsonResult.withThrowingParser System.Convert.FromBase64String) ofBytes

        let Property_ k =
            Optic.compose Object_ (JsonObject.Optics.key_ k)

    module Operators =
        let inline (>>=) m f = bind f m
        let inline (=<<) f m = bind f m
        let inline (<*>) f m = apply m f
        let inline (<!>) f m = map f m
        let inline ( *>) m1 m2 = map2 (fun _ x -> x) m1 m2
        let inline ( <*) m1 m2 = map2 (fun x _ -> x) m1 m2
        let (>=>) m1 m2 = m1 >> bind m2
        let (<=<) m2 m1 = m1 >> bind m2

type ChironDefaults with
    static member FromJson (): JsonReader<unit> =
        Optic.get Json.Optics.Null_

    static member FromJson (_:bool): JsonReader<bool> =
        Optic.get Json.Optics.Bool_

    static member FromJson (_:int16): JsonReader<int16> =
        Optic.get Json.Optics.Int16_

    static member FromJson (_:int): JsonReader<int> =
        Optic.get Json.Optics.Int32_

    static member FromJson (_:int64): JsonReader<int64> =
        Optic.get Json.Optics.Int64_

    static member FromJson (_:uint16): JsonReader<uint16> =
        Optic.get Json.Optics.UInt16_

    static member FromJson (_:uint32): JsonReader<uint32> =
        Optic.get Json.Optics.UInt32_

    static member FromJson (_:uint64): JsonReader<uint64> =
        Optic.get Json.Optics.UInt64_

    static member FromJson (_:single): JsonReader<single> =
        Optic.get Json.Optics.Single_

    static member FromJson (_:float): JsonReader<float> =
        Optic.get Json.Optics.Float_

    static member FromJson (_:decimal): JsonReader<decimal> =
        Optic.get Json.Optics.Decimal_

    static member FromJson (_:bigint): JsonReader<bigint> =
        Optic.get Json.Optics.BigInt_

    static member FromJson (_:System.DateTime): JsonReader<System.DateTime> =
        Optic.get Json.Optics.DateTime_

    static member FromJson (_:System.DateTimeOffset): JsonReader<System.DateTimeOffset> =
        Optic.get Json.Optics.DateTimeOffset_

    static member FromJson (_:System.Guid): JsonReader<System.Guid> =
        Optic.get Json.Optics.Guid_

    static member FromJson (_:byte array): JsonReader<byte array> =
        Optic.get Json.Optics.Bytes_

    static member FromJson (_:string): JsonReader<string> =
        Optic.get Json.Optics.String_

    static member FromJson (_:Json): JsonReader<Json> =
        Ok

    static member ToJson (): Json =
        Json.Null

    static member ToJson b: Json =
        Json.ofBool b

    static member ToJson n: Json =
        Json.ofInt16 n

    static member ToJson n: Json =
        Json.ofInt32 n

    static member ToJson n: Json =
        Json.ofInt64 n

    static member ToJson n: Json =
        Json.ofUInt16 n

    static member ToJson n: Json =
        Json.ofUInt32 n

    static member ToJson n: Json =
        Json.ofUInt64 n

    static member ToJson n: Json =
        Json.ofSingle n

    static member ToJson n: Json =
        Json.ofFloat n

    static member ToJson n: Json =
        Json.ofDecimal n

    static member ToJson n: Json =
        Json.ofBigInt n

    static member ToJson s: Json =
        Json.ofString s

    static member ToJson dt: Json =
        Json.ofDateTime dt

    static member ToJson dt: Json =
        Json.ofDateTimeOffset dt

    static member ToJson g: Json =
        Json.ofGuid g

    static member ToJson bs: Json =
        Json.ofBytes bs

    static member ToJson j: Json =
        j

[<AutoOpen>]
module Serialization =
    module Json =
        let deserializeWith deser: JsonReader<'a> = deser Unchecked.defaultof<'a>

        let inline deserializeWithDefaults (defaults : ^def) (dummy : ^a): JsonReader<'a> =
            ((^a or ^def) : (static member FromJson : ^a -> JsonReader<'a>) dummy)

        let inline deserialize (json: Json) =
            deserializeWithDefaults ChironDefaults Unchecked.defaultof<'a> json

        let inline deserializeList init fold xs =
            List.fold (function
                | Error e -> fun _ -> Error e
                | Ok xs -> fun json ->
                    match deserialize json with
                    | Ok x -> Ok (fold x xs)
                    | Error e -> Error e) (Ok init) xs

        let inline serializeWithDefaults (defaults : ^def) (a: 'a): Json =
            ((^a or ^def) : (static member ToJson : ^a -> Json) a)

        let inline serialize (a: 'a) =
            serializeWithDefaults ChironDefaults a

        let inline encode (a: 'a): Json =
            (^a : (static member Encode: ^a * JsonObject -> JsonObject) (a, JsonObject.empty))
            |> JsonObject.toJson

type ChironDefaults with
    static member inline FromJson (_:'a array): JsonReader<'a array> =
        let init xs : 'a array * int = (Array.zeroCreate (List.length xs), 0)
        let fold x (arr: 'a array, idx) =
            arr.[idx] <- x
            (arr, idx + 1)
        let doDeser xs = Serialization.Json.deserializeList (init xs) fold xs
        Optic.get Json.Optics.Array_
        >> Result.bind doDeser
        >> Result.map fst

    static member inline FromJson (_:'a list): JsonReader<'a list> =
        let init = []
        let fold x xs = x :: xs
        Optic.get Json.Optics.Array_
        >> Result.bind (List.rev >> Serialization.Json.deserializeList init fold)

    static member inline FromJson (_:Set<'a>): JsonReader<Set<'a>> =
        let init = Set.empty
        let fold = Set.add
        Optic.get Json.Optics.Array_
        >> Result.bind (Serialization.Json.deserializeList init fold)

    static member inline FromJson (_:Map<string,'a>): JsonReader<Map<string,'a>> =
        let doDeser xs =
            let k, v = List.unzip xs
            List.rev v |> Serialization.Json.deserializeList [] (fun x xs -> x :: xs)
            |> Result.map (fun jsons -> List.zip k jsons |> Map.ofList)
        Optic.get Json.Optics.Object_
        >> Result.bind (JsonObject.toPropertyList >> doDeser)

    static member inline FromJson (_:'a option): JsonReader<'a option> =
        fun json ->
            Optic.get Json.Optics.Null_ json
            |> function
                | Ok () -> Ok None
                | _ -> Serialization.Json.deserialize json |> Result.map Some

    static member inline FromJson (_:'a * 'b): JsonReader<'a * 'b> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>) and (^b or ChironDefaults) : (static member FromJson : ^b -> JsonReader<'b>) =
        let binder: Json list -> JsonResult<'a * 'b> = function
            | [a;b] ->
                Serialization.Json.deserialize a
                |> Result.map (fun a b -> a, b)
                |> JsonResult.applyWith Serialization.Json.deserialize b
            | [_] -> Error [DeserializationError (typeof<'a * 'b>, "2-Tuple has only one element")]
            | [] -> Error [DeserializationError (typeof<'a * 'b>, "2-Tuple has zero elements")]
            | _ -> Error [DeserializationError (typeof<'a * 'b>, "2-Tuple has too many elements")]

        Optic.get Json.Optics.Array_
        >> Result.bind binder

    static member inline FromJson (_:'a * 'b * 'c): JsonReader<'a * 'b * 'c> =
        let binder: Json list -> JsonResult<'a * 'b * 'c> = function
            | [a;b;c] ->
                Result.map (fun a b c -> a, b, c) (Serialization.Json.deserialize a)
                |> JsonResult.applyWith Serialization.Json.deserialize b
                |> JsonResult.applyWith Serialization.Json.deserialize c
            | l when List.length l < 3 -> Error [DeserializationError (typeof<'a * 'b * 'c>, "3-Tuple has insufficient elements")]
            | _ -> Error [DeserializationError (typeof<'a * 'b * 'c>, "3-Tuple has too many elements")]

        Optic.get Json.Optics.Array_
        >> Result.bind binder

    static member inline FromJson (_:'a * 'b * 'c * 'd): JsonReader<'a * 'b * 'c * 'd> =
        let binder: Json list -> JsonResult<'a * 'b * 'c * 'd> = function
            | [a;b;c;d] ->
                Result.map (fun a b c d -> a, b, c, d) (Serialization.Json.deserialize a)
                |> JsonResult.applyWith Serialization.Json.deserialize b
                |> JsonResult.applyWith Serialization.Json.deserialize c
                |> JsonResult.applyWith Serialization.Json.deserialize d
            | l when List.length l < 4 -> Error [DeserializationError (typeof<'a * 'b * 'c * 'd>, "4-Tuple has insufficient elements")]
            | _ -> Error [DeserializationError (typeof<'a * 'b * 'c * 'd>, "4-Tuple has too many elements")]

        Optic.get Json.Optics.Array_
        >> Result.bind binder

    static member inline FromJson (_:'a * 'b * 'c * 'd * 'e): JsonReader<'a * 'b * 'c * 'd * 'e> =
        let binder: Json list -> JsonResult<'a * 'b * 'c * 'd * 'e> = function
            | [a;b;c;d;e] ->
                Result.map (fun a b c d e -> a, b, c, d, e) (Serialization.Json.deserialize a)
                |> JsonResult.applyWith Serialization.Json.deserialize b
                |> JsonResult.applyWith Serialization.Json.deserialize c
                |> JsonResult.applyWith Serialization.Json.deserialize d
                |> JsonResult.applyWith Serialization.Json.deserialize e
            | l when List.length l < 5 -> Error [DeserializationError (typeof<'a * 'b * 'c * 'd * 'e>, "5-Tuple has insufficient elements")]
            | _ -> Error [DeserializationError (typeof<'a * 'b * 'c * 'd * 'e>, "5-Tuple has too many elements")]

        Optic.get Json.Optics.Array_
        >> Result.bind binder

    static member inline FromJson (_:'a * 'b * 'c * 'd * 'e * 'f): JsonReader<'a * 'b * 'c * 'd * 'e * 'f> =
        let binder: Json list -> JsonResult<'a * 'b * 'c * 'd * 'e * 'f> = function
            | [a;b;c;d;e;f] ->
                Result.map (fun a b c d e f -> a, b, c, d, e, f) (Serialization.Json.deserialize a)
                |> JsonResult.applyWith Serialization.Json.deserialize b
                |> JsonResult.applyWith Serialization.Json.deserialize c
                |> JsonResult.applyWith Serialization.Json.deserialize d
                |> JsonResult.applyWith Serialization.Json.deserialize e
                |> JsonResult.applyWith Serialization.Json.deserialize f
            | l when List.length l < 6 -> Error [DeserializationError (typeof<'a * 'b * 'c * 'd * 'e * 'f>, "6-Tuple has insufficient elements")]
            | _ -> Error [DeserializationError (typeof<'a * 'b * 'c * 'd * 'e * 'f>, "6-Tuple has too many elements")]

        Optic.get Json.Optics.Array_
        >> Result.bind binder

    static member inline FromJson (_:'a * 'b * 'c * 'd * 'e * 'f * 'g): JsonReader<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
        let binder: Json list -> JsonResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> = function
            | [a;b;c;d;e;f;g] ->
                Result.map (fun a b c d e f g -> a, b, c, d, e, f, g) (Serialization.Json.deserialize a)
                |> JsonResult.applyWith Serialization.Json.deserialize b
                |> JsonResult.applyWith Serialization.Json.deserialize c
                |> JsonResult.applyWith Serialization.Json.deserialize d
                |> JsonResult.applyWith Serialization.Json.deserialize e
                |> JsonResult.applyWith Serialization.Json.deserialize f
                |> JsonResult.applyWith Serialization.Json.deserialize g
            | l when List.length l < 7 -> Error [DeserializationError (typeof<'a * 'b * 'c * 'd * 'e * 'f * 'g>, "7-Tuple has insufficient elements")]
            | _ -> Error [DeserializationError (typeof<'a * 'b * 'c * 'd * 'e * 'f * 'g>, "7-Tuple has too many elements")]

        Optic.get Json.Optics.Array_
        >> Result.bind binder

    static member inline ToJson (xs: 'a list): Json =
        List.map Serialization.Json.serialize xs
        |> Json.ofList

    static member inline ToJson (xs: 'a array): Json =
        Array.map Serialization.Json.serialize xs
        |> List.ofArray
        |> Json.ofList

    static member inline ToJson (xs: 'a option): Json =
        match xs with
        | None -> Json.``null``
        | Some x -> Serialization.Json.serialize x

    static member inline ToJson (xs: Set<'a>): Json =
        Set.toList xs
        |> List.map Serialization.Json.serialize
        |> Json.ofList

    static member inline ToJson (kvps: Map<string,'a>): Json =
        Map.map (fun k v -> Serialization.Json.serialize v) kvps
        |> JsonObject.ofMap
        |> Json.ofJsonObject

    static member inline ToJson ((a, b)): Json =
        Json.ofList
            [ Serialization.Json.serialize a
              Serialization.Json.serialize b ]

    static member inline ToJson ((a, b, c)): Json =
        Json.ofList
            [ Serialization.Json.serialize a
              Serialization.Json.serialize b
              Serialization.Json.serialize c ]

    static member inline ToJson ((a, b, c, d)): Json =
        Json.ofList
            [ Serialization.Json.serialize a
              Serialization.Json.serialize b
              Serialization.Json.serialize c
              Serialization.Json.serialize d ]

    static member inline ToJson ((a, b, c, d, e)): Json =
        Json.ofList
            [ Serialization.Json.serialize a
              Serialization.Json.serialize b
              Serialization.Json.serialize c
              Serialization.Json.serialize d
              Serialization.Json.serialize e ]

    static member inline ToJson ((a, b, c, d, e, f)): Json =
        Json.ofList
            [ Serialization.Json.serialize a
              Serialization.Json.serialize b
              Serialization.Json.serialize c
              Serialization.Json.serialize d
              Serialization.Json.serialize e
              Serialization.Json.serialize f ]

    static member inline ToJson ((a, b, c, d, e, f, g)): Json =
        Json.ofList
            [ Serialization.Json.serialize a
              Serialization.Json.serialize b
              Serialization.Json.serialize c
              Serialization.Json.serialize d
              Serialization.Json.serialize e
              Serialization.Json.serialize f
              Serialization.Json.serialize g ]

[<AutoOpen>]
module Parsing =
    module Parser =
        open FParsec

        let ws   = spaces // eats any whitespace
        let inline str s = pstring s

        let stringLiteral : Parser<string,unit> =
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
        let jnumber : Parser<Json,unit> =
            fun stream ->
                let reply = numberLiteralE jsonNumOpts (ErrorMessageList(ErrorMessage.Expected("JSON number"))) stream
                if reply.Status = ReplyStatus.Ok then
                    Reply(Json.Number reply.Result.String)
                else
                    Reply(reply.Status, reply.Error)

        let jtrue  = stringReturn "true"  Json.True
        let jfalse = stringReturn "false" Json.False
        let jnull  = stringReturn "null" Json.Null

        // jvalue, jlist and jobject are three mutually recursive grammar productions.
        // In order to break the cyclic dependency, we make jvalue a parser that
        // forwards all calls to a parser in a reference cell.
        let jvalue, jvalueRef = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser

        let listBetweenStrings sOpen sClose pElement f =
            between (str sOpen) (str sClose) (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

        let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)

        let jlist   = listBetweenStrings "[" "]" jvalue Json.Array
        let toJsonObject = fun lst -> let map = Map.ofList lst in ReadObject (lst,map) |> Json.Object
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

    module Json =
        let tryParse s =
            if System.String.IsNullOrWhiteSpace s then
                Error [NoInput]
            else
                Parser.parseJsonString s
                |> Parser.handleParserResult

        let parseOrThrow s =
            match tryParse s with
            | Ok json -> json
            | Error e -> failwith (List.map JsonFailure.toString e |> String.concat "\n")

        let import s =
            fun json ->
                match tryParse s with
                | Ok json' -> Ok (), json'
                | Error e -> Error e, json

        let tryParseStream (stream : #System.IO.Stream) =
            Parser.parseJsonStream stream
            |> Parser.handleParserResult

[<AutoOpen>]
module Formatting =
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

    type PropertyNameSpacing =
        | NoSpaceBetweenNameAndValue
        | SpaceBetweenNameAndValue

    type ElementSpacing =
        | NoSpaceBetweenElements
        | SpaceBetweenElements
        | NewLineBetweenElements of indentSpaces:int

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
          ElementSpacing = NewLineBetweenElements 2 }

    module StringBuilder =
        open System.Text

        let inline append (sb: StringBuilder) (s: string) =
            sb.Append s |> ignore
        let inline appendSubstr (sb: StringBuilder) (s: string) (start: int) (count: int) =
            sb.Append (s, start, count) |> ignore
        let inline appendChar (sb: StringBuilder) (c: char) =
            sb.Append c |> ignore
        let inline appendCharRep (sb: StringBuilder) (c: char) (repeats: int) =
            sb.Append (c, repeats) |> ignore

        let writeString (sb:System.Text.StringBuilder) (cs:string) =
            let rec escapeState index =
                append sb (escaped cs.[index])
                let nextIndex = index + 1
                if nextIndex < cs.Length then
                    if isEscapeChar cs.[nextIndex] |> not then
                        coreState nextIndex
                    else
                        escapeState nextIndex
            and coreState index =
                let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
                if nextEscapeIndex = -1 then
                    appendSubstr sb cs index (cs.Length - index)
                else
                    appendSubstr sb cs index (nextEscapeIndex - index)
                    escapeState nextEscapeIndex
            coreState 0

        let appendSpaceBetweenElements sb { ElementSpacing = es } level =
            match es, level with
            | NoSpaceBetweenElements, _ -> ()
            | SpaceBetweenElements, _ -> appendChar sb ' '
            | NewLineBetweenElements _, 0 -> appendChar sb '\n'
            | NewLineBetweenElements s, l -> appendChar sb '\n'; appendCharRep sb ' ' (s * l |> int)

        let addPropertyNameSeparator sb { PropertyNameSpacing = pns } =
            match pns with
            | NoSpaceBetweenNameAndValue -> appendChar sb ':'
            | SpaceBetweenNameAndValue -> append sb ": "

        let rec formatJson sb options level = function
            | Json.Object (WriteObject props)
            | Json.Object (ReadObject (props,_)) -> formatObject sb options level props
            | Json.Array elems -> formatArray sb options level elems
            | Json.String s -> formatString sb s
            | Json.Number x -> formatNumber sb x
            | Json.True -> append sb "true"
            | Json.False -> append sb "false"
            | Json.Null -> append sb "null"

        and formatObject sb options level props =
            let newLevel = level + 1
            appendChar sb '{'
            appendSpaceBetweenElements sb options newLevel
            joinObject sb options newLevel props
            appendSpaceBetweenElements sb options level
            appendChar sb '}'

        and joinObject sb options level values =
            match values with
            | [] -> ()
            | [v] -> formatProperty sb options level v
            | v :: vs ->
                joinObject sb options level vs
                appendChar sb ','
                appendSpaceBetweenElements sb options level
                formatProperty sb options level v

        and formatProperty sb options level (k,v) =
            formatString sb k
            addPropertyNameSeparator sb options
            formatJson sb options level v

        and formatArray  sb options level elems =
            let newLevel = level + 1
            appendChar sb '['
            appendSpaceBetweenElements sb options newLevel
            joinArray sb options newLevel elems
            appendSpaceBetweenElements sb options level
            appendChar sb ']'

        and joinArray sb options level values =
            match values with
            | [] -> ()
            | [v] -> formatJson sb options level v
            | v :: vs ->
                formatJson sb options level v
                appendChar sb ','
                appendSpaceBetweenElements sb options level
                joinArray sb options level vs

        and formatString sb str =
            appendChar sb '"'
            writeString sb str
            appendChar sb '"'

        and formatNumber sb n =
            append sb n

    [<RequireQualifiedAccess>]
    module Json =
        let formatWith options json =
            let sb = System.Text.StringBuilder ()
            StringBuilder.formatJson sb options 0 json
            sb.ToString()

        let format json =
            formatWith JsonFormattingOptions.Compact json

type Json with
    override x.ToString() =
        Formatting.Json.format x

[<RequireQualifiedAccess>]
module ObjectReader =
    let init (a: 'a) : ObjectReader<'a> =
        fun json -> Ok a

    let error (e: JsonFailure) : ObjectReader<'a> =
        fun json -> Error [e]

    let ofResult result : ObjectReader<_> =
        fun json -> result

    let bind (a2bD: 'a -> ObjectReader<'b>) (aD: ObjectReader<'a>) : ObjectReader<'b> =
        fun json ->
            match aD json with
            | Ok a -> a2bD a json
            | Error es -> Error es

    let apply (aD: ObjectReader<'a>) (a2Db: ObjectReader<'a -> 'b>) : ObjectReader<'b> =
        fun json ->
            match a2Db json, aD json with
            | Ok a2b, Ok a -> Ok (a2b a)
            | Error e, Ok _
            | Ok _, Error e -> Error e
            | Error [e1], Error [e2] -> Error [e1; e2]
            | Error es1, Error es2 -> Error (es1 @ es2)

    let map (a2b: 'a -> 'b) (aD: ObjectReader<'a>) : ObjectReader<'b> =
        fun json ->
            match aD json with
            | Ok a -> Ok (a2b a)
            | Error e -> Error e

    let map2 (a2b2c: 'a -> 'b -> 'c) (aD: ObjectReader<'a>) (bD: ObjectReader<'b>) : ObjectReader<'c> =
        map a2b2c aD
        |> apply bD

    let map3 (a2b2c2d: 'a -> 'b -> 'c -> 'd) (aD: ObjectReader<'a>) (bD: ObjectReader<'b>) (cD: ObjectReader<'c>) : ObjectReader<'d> =
        map a2b2c2d aD
        |> apply bD
        |> apply cD

    let map4 (a2b2c2d2x: 'a -> 'b -> 'c -> 'd -> 'x) (aD: ObjectReader<'a>) (bD: ObjectReader<'b>) (cD: ObjectReader<'c>) (dD: ObjectReader<'d>) : ObjectReader<'x> =
        map a2b2c2d2x aD
        |> apply bD
        |> apply cD
        |> apply dD

    let map5 (a2b2c2d2x2y: 'a -> 'b -> 'c -> 'd -> 'x -> 'y) (aD: ObjectReader<'a>) (bD: ObjectReader<'b>) (cD: ObjectReader<'c>) (dD: ObjectReader<'d>) (xD: ObjectReader<'x>) : ObjectReader<'y> =
        map a2b2c2d2x2y aD
        |> apply bD
        |> apply cD
        |> apply dD
        |> apply xD

    let map6 (a2b2c2d2x2y2z: 'a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) (aD: ObjectReader<'a>) (bD: ObjectReader<'b>) (cD: ObjectReader<'c>) (dD: ObjectReader<'d>) (xD: ObjectReader<'x>) (yD: ObjectReader<'y>) : ObjectReader<'z> =
        map a2b2c2d2x2y2z aD
        |> apply bD
        |> apply cD
        |> apply dD
        |> apply xD
        |> apply yD

    let toJsonReader (f: ObjectReader<'a>) : JsonReader<'a> =
        Optic.get Json.Optics.Object_
        >> Result.bind f

    module Operators =
        let inline (>>=) m f = bind f m
        let inline (=<<) f m = bind f m
        let inline (<*>) f m = apply m f
        let inline (<!>) f m = map f m
        let inline ( *>) m1 m2 = map2 (fun _ x -> x) m1 m2
        let inline ( <*) m1 m2 = map2 (fun x _ -> x) m1 m2
        let (>=>) m1 m2 = m1 >> bind m2
        let (<=<) m2 m1 = m1 >> bind m2

[<RequireQualifiedAccess>]
module JsonReader =
    let toJson (aD: JsonReader<'a>) : Json<'a> =
        fun json ->
            aD json, json

    let init (a: 'a) : JsonReader<'a> =
        fun json -> Ok a

    let error (e: JsonFailure) : JsonReader<'a> =
        fun json -> Error [e]

    let ofResult result : JsonReader<'a> =
        fun json -> result

    let bind (a2bD: 'a -> JsonReader<'b>) (aD: JsonReader<'a>) : JsonReader<'b> =
        fun json ->
            match aD json with
            | Ok a -> a2bD a json
            | Error es -> Error es

    let apply (aD: JsonReader<'a>) (a2Db: JsonReader<'a -> 'b>) : JsonReader<'b> =
        fun json ->
            match a2Db json, aD json with
            | Ok a2b, Ok a -> Ok (a2b a)
            | Error e, Ok _
            | Ok _, Error e -> Error e
            | Error [e1], Error [e2] -> Error [e1; e2]
            | Error es1, Error es2 -> Error (es1 @ es2)

    let map (a2b: 'a -> 'b) (aD: JsonReader<'a>) : JsonReader<'b> =
        fun json ->
            match aD json with
            | Ok a -> Ok (a2b a)
            | Error e -> Error e

    let map2 (a2b2c: 'a -> 'b -> 'c) (aD: JsonReader<'a>) (bD: JsonReader<'b>) : JsonReader<'c> =
        map a2b2c aD
        |> apply bD

    let map3 (a2b2c2d: 'a -> 'b -> 'c -> 'd) (aD: JsonReader<'a>) (bD: JsonReader<'b>) (cD: JsonReader<'c>) : JsonReader<'d> =
        map a2b2c2d aD
        |> apply bD
        |> apply cD

    let map4 (a2b2c2d2x: 'a -> 'b -> 'c -> 'd -> 'x) (aD: JsonReader<'a>) (bD: JsonReader<'b>) (cD: JsonReader<'c>) (dD: JsonReader<'d>) : JsonReader<'x> =
        map a2b2c2d2x aD
        |> apply bD
        |> apply cD
        |> apply dD

    let map5 (a2b2c2d2x2y: 'a -> 'b -> 'c -> 'd -> 'x -> 'y) (aD: JsonReader<'a>) (bD: JsonReader<'b>) (cD: JsonReader<'c>) (dD: JsonReader<'d>) (xD: JsonReader<'x>) : JsonReader<'y> =
        map a2b2c2d2x2y aD
        |> apply bD
        |> apply cD
        |> apply dD
        |> apply xD

    let map6 (a2b2c2d2x2y2z: 'a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) (aD: JsonReader<'a>) (bD: JsonReader<'b>) (cD: JsonReader<'c>) (dD: JsonReader<'d>) (xD: JsonReader<'x>) (yD: JsonReader<'y>) : JsonReader<'z> =
        map a2b2c2d2x2y2z aD
        |> apply bD
        |> apply cD
        |> apply dD
        |> apply xD
        |> apply yD

[<AutoOpen>]
module Builders =
    type JsonBuilder () =
        member __.Return (a : 'a) : Json<'a> = Json.init a
        member __.ReturnFrom (aJ) : Json<'a> = aJ
        member __.Bind (aJ, a2bJ) : Json<'a> = aJ |> Json.bind a2bJ
        member __.Zero () : Json<unit> = Json.init ()
        member __.Combine (m1, m2) : Json<'a> = m1 |> Json.bind (fun _ -> m2)
        member __.Delay (u2bJ) : Json<'a> = Json.init () |> Json.bind u2bJ

    type JsonReaderBuilder() =
        member __.Return (a: 'a) : ObjectReader<'a> = ObjectReader.init a
        member __.ReturnFrom (aD: ObjectReader<'a>) = aD
        member __.Bind (aM, a2bM) : ObjectReader<'a> = aM |> ObjectReader.bind a2bM
        member __.Zero () : ObjectReader<unit> = ObjectReader.init ()
        member __.Combine(r1, r2) : ObjectReader<'a> = r1 |> ObjectReader.bind (fun _ -> r2)
        // member __.Delay(u2bM) : ObjectReader<'a> = ObjectReader.init () |> ObjectReader.bind u2bM

        member __.Run (aM : ObjectReader<'a>) : JsonReader<'a> =
            ObjectReader.toJsonReader aM

    type JsonWriterBuilder () =
        member __.Zero () = JsonObject.empty
        member __.Bind (j2j: JsonObject -> JsonObject, jObj: JsonObject) : JsonObject = j2j jObj
        member __.Run (jObj: JsonObject) : Json = JsonObject.toJson jObj

    let json = JsonBuilder ()
    let jsonReader = JsonReaderBuilder()
    let jsonWriter = JsonWriterBuilder()

[<AutoOpen>]
module Patterns =
    let (|Object|Array|String|Number|Bool|Null|) = function
        | Json.Object o -> Object o
        | Json.Array a -> Array a
        | Json.String s -> String s
        | Json.Number n -> Number n
        | Json.True -> Bool true
        | Json.False -> Bool false
        | Json.Null -> Null

    let (|PropertyWith|_|) (r: JsonReader<'a>) (key: string) (json: Json) : 'a option =
        Optic.get (Json.Optics.Property_ key) json
        |> Result.bind r
        |> function
            | Result.Ok x -> Some x
            | _ -> None

    let inline (|Property|_|) (key: string) (json: Json): 'a option =
        Optic.get (Json.Optics.Property_ key) json
        |> Result.bind Serialization.Json.deserialize
        |> function
            | Result.Ok x -> Some x
            | _ -> None
