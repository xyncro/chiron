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
    | Number of number:string
    | True
    | False
    | Null

and [<CustomEquality;NoComparison>] JsonObject =
    | WriteObject of propList: (string * Json) list
    | ReadObject of propList: (string * Json) list * propMap: Map<string,Json>
    override x.Equals(o) =
        match o with
        | :? JsonObject as y -> (x :> System.IEquatable<JsonObject>).Equals(y)
        | _ -> false
    override x.GetHashCode() =
        match x with
        | WriteObject ps -> Map.ofList (List.rev ps) |> hash
        | ReadObject (_, mps) -> mps |> hash
    interface System.IEquatable<JsonObject> with
        member x.Equals(y) =
            match x, y with
            | WriteObject xps, WriteObject yps -> Map.ofList (List.rev xps) = Map.ofList (List.rev yps)
            | ReadObject (_, mps), WriteObject ps -> Map.ofList (List.rev ps) = mps
            | WriteObject ps, ReadObject (_, mps) -> Map.ofList (List.rev ps) = mps
            | ReadObject (_, xps), ReadObject (_, yps) -> xps = yps

type JsonTag =
    | PropertyTag of propertyName: string
    | IndexTag of index: uint32
    | ChoiceTag of choice: uint32
type JsonFailure =
    | Tagged of tag: JsonTag * failure: JsonFailure
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonTag =
    let toString = function
        | PropertyTag p -> p
        | IndexTag i -> System.String.Concat ("[", string i, "]")
        | ChoiceTag c -> System.String.Concat ("(Choice #", string (c + 1u), ")")

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonFailure =
    let rec toString = function
        | Tagged (t, (Tagged (PropertyTag(_),_) as f)) -> System.String.Concat (JsonTag.toString t, ".", toString f)
        | Tagged (t, (Tagged _ as f)) -> System.String.Concat (JsonTag.toString t, toString f)
        | Tagged (p,f) -> System.String.Concat (p, ": ", toString f)
        | NoInput -> "No input was provided"
        | PropertyNotFound -> "Failed to find expected property"
        | TypeMismatch (e,a) -> System.String.Concat ("Expected to find ", JsonMemberType.describe e, ", but instead found ", JsonMemberType.describe a)
        | DeserializationError (t,e) -> System.String.Concat ("Unable to deserialize value as '", t.FullName, "': ", e)
        | ParserFailure e -> "Invalid JSON, failed to parse: " + e

    let tag (t: JsonTag) (f: JsonFailure) =
        Tagged (t, f)

    let tagList (t: JsonTag) (fs: JsonFailure list) =
        List.map (tag t) fs

[<RequireQualifiedAccess>]
module JsonResult =
    let emptyError : JsonResult<'a> = Error []

    let ok x : JsonResult<'a> = Ok x

    let noInput : JsonResult<'a> =
        Error [NoInput]

    let parserFailure (err: string) : JsonResult<'a> =
        Error [ParserFailure err]

    let propertyNotFound k : JsonResult<'a> =
        Error [Tagged (PropertyTag k, PropertyNotFound)]

    let deserializationError<'a> (err: string) : JsonResult<'a> =
        Error [DeserializationError (typeof<'a>, err)]

    let typeMismatch<'a> (expected: JsonMemberType) (actual: Json) : JsonResult<'a> =
        Error [TypeMismatch (expected, JsonMemberType.ofJson actual)]

    let bind (a2bR: 'a -> JsonResult<'b>) (aR : JsonResult<'a>) : JsonResult<'b> = Result.bind a2bR aR
    let map (a2b: 'a -> 'b) (aR : JsonResult<'a>) : JsonResult<'b> = Result.map a2b aR
    let mapError (jfs2e: JsonFailure list -> 'e) (aR : JsonResult<'a>) : Result<'a,'e> = Result.mapError jfs2e aR
    let mapErrors (jf2e: JsonFailure -> 'e) (aR : JsonResult<'a>) : Result<'a,'e list> = Result.mapError (List.map jf2e) aR

    let apply (aR: JsonResult<'a>) (a2Rb: JsonResult<'a -> 'b>) : JsonResult<'b> =
        match a2Rb, aR with
        | Ok a2b, Ok a -> Ok (a2b a)
        | Error e1, Error e2 -> Error (e1 @ e2)
        | Error e, _ | _, Error e -> Error e

    let applyShort (c2aR: 'c -> JsonResult<'a>) (c: 'c) (a2Rb: JsonResult<'a -> 'b>) : JsonResult<'b> =
        match a2Rb with
        | Ok a2b ->
            match c2aR c with
            | Ok a -> Ok (a2b a)
            | Error e -> Error e
        | Error e -> Error e

    let withPropertyTag p (a2bR : 'a -> JsonResult<'b>) (a : 'a) : JsonResult<'b> =
        match a2bR a with
        | Ok a -> Ok a
        | Error errs ->
            JsonFailure.tagList (PropertyTag p) errs
            |> Error

    let withIndexTag i (a2bR : 'a -> JsonResult<'b>) (a : 'a) : JsonResult<'b> =
        match a2bR a with
        | Ok a -> Ok a
        | Error errs ->
            JsonFailure.tagList (IndexTag i) errs
            |> Error

    let withChoiceTag c (a2bR : 'a -> JsonResult<'b>) (a : 'a) : JsonResult<'b> =
        match a2bR a with
        | Ok a -> Ok a
        | Error errs ->
            JsonFailure.tagList (ChoiceTag c) errs
            |> Error

    let fromThrowingConverter (convert: 'a -> 'b) (a: 'a): JsonResult<'b> =
        try
            Ok (convert a)
        with e -> deserializationError e.Message

    let getOrThrow : JsonResult<'a> -> 'a = function
        | Ok x -> x
        | Error [] -> failwith "JSON error"
        | Error e -> failwith (List.map JsonFailure.toString e |> String.concat "\n")

    let summarize : JsonResult<'a> -> string = function
        | Ok _ -> "No errors"
        | Error [] -> "In error state with no reasons given"
        | Error [e] -> sprintf "Found 1 error:\n  %s" <| JsonFailure.toString e
        | Error errs ->
            let sb = System.Text.StringBuilder()
            let sb = sb.AppendLine(sprintf "Found %i errors:" (List.length errs))
            let sb = errs |> List.fold (fun (sb:System.Text.StringBuilder) e -> sb.Append("  ").AppendLine(JsonFailure.toString e)) sb
            sb.ToString()

module Internal =
    let jsonToJsonObject json =
        match json with
        | Json.Object o -> Ok o
        | _ -> JsonResult.typeMismatch JsonMemberType.Object json

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
        fun json ->
            Internal.jsonToJsonObject json
            |> JsonResult.bind f

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
            | Error [e1], Error [e2] -> Error [e1; e2]
            | Error es1, Error es2 -> Error (es1 @ es2)
            | Error e, _ | _, Error e -> Error e

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

    let toJson (aD: JsonReader<'a>) : Json<'a> =
        fun json ->
            aD json, json

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonObject =
    let empty = WriteObject []

    module PropertyList =
        let inline add k v ps = (k, v) :: ps
        let remove k ps = List.filter (fun kvp -> fst kvp <> k) ps
        let upsert k v ps =
            let mutable found = false
            let newList = List.map (fun kvp -> if fst kvp = k then found <- true; (fst kvp, v) else kvp) ps
            if not found then
                add k v ps
            else
                newList

        let tryFind k ps = List.tryPick (fun kvp -> if fst kvp = k then Some (snd kvp) else None) ps

    module PropertyMap =
        let inline add k v ps = Map.add k v ps
        let inline remove k ps = Map.remove k ps
        let inline upsert k v ps = Map.add k v ps

        let inline tryFind k ps = Map.tryFind k ps

    let add k v = function
        | WriteObject ps -> WriteObject (PropertyList.add k v ps)
        | ReadObject (ps, mps) ->
            ReadObject (PropertyList.add k v ps, PropertyMap.add k v mps)

    let remove k = function
        | WriteObject ps -> WriteObject (PropertyList.remove k ps)
        | ReadObject (ps, mps) -> ReadObject (PropertyList.remove k ps, PropertyMap.remove k mps)

    let upsert k v = function
        | WriteObject ps -> WriteObject (PropertyList.upsert k v ps)
        | ReadObject (ps, mps) ->
            ReadObject (PropertyList.upsert k v ps, PropertyMap.upsert k v mps)

    let tryFind k = function
        | WriteObject ps -> PropertyList.tryFind k ps
        | ReadObject (_, mps) -> PropertyMap.tryFind k mps

    let find k jsonObj =
        match tryFind k jsonObj with
        | Some v -> Ok v
        | None -> JsonResult.propertyNotFound k

    let optimizeRead = function
        | WriteObject ps -> ReadObject (ps, Map.ofList (List.rev ps))
        | o -> o

    let optimizeAppend = function
        | ReadObject (ps, mps) -> WriteObject ps
        | o -> o

    let mapToList mps = Map.toList mps |> List.rev
    let listToMap ps = List.rev ps |> Map.ofList

    let dedupeWithMap kvps m =
        if List.length kvps = Map.count m then
            kvps
        else
            let hs = System.Collections.Generic.HashSet ()
            List.foldBack (fun (k,_) s -> if hs.Add(k) then (k, Map.find k m) :: s else s) kvps []

    let dedupeList kvps =
        dedupeWithMap kvps (listToMap kvps)

    let removeDuplicates = function
        | WriteObject ps -> WriteObject (dedupeList ps)
        | ReadObject (ps, mps) ->
            if List.length ps <> Map.count mps then
                ReadObject (dedupeWithMap ps mps, mps)
            else
                ReadObject (ps, mps)

    let buildWith (encode: ObjectWriter<'a>) (a: 'a): Json =
        encode a empty
        |> Json.Object

    let writeWith (encode : JsonWriter<'a>) (k : string) (a: 'a) (jsonObject : JsonObject) =
        add k (encode a) jsonObject

    let writeOptionalWith (encode : JsonWriter<'a>) (k : string) (aO : 'a option) (jsonObject : JsonObject) =
        match aO with
        | Some a -> writeWith encode k a jsonObject
        | None -> jsonObject

    let writeChildWith (encode: ObjectWriter<'a>) (k: string) (a: 'a) (jsonObject: JsonObject) : JsonObject =
        add k (buildWith encode a) jsonObject

    let writeOptionalChildWith (encode: ObjectWriter<'a>) (k: string) (aO: 'a option) (jsonObject: JsonObject) : JsonObject =
        match aO with
        | Some a -> writeChildWith encode k a jsonObject
        | None -> jsonObject

    let writeMixinWith (encode: ObjectWriter<'a>) a jObj =
        encode a jObj

    let writeOptionalMixinWith encode aO jObj =
        match aO with
        | Some a -> writeMixinWith encode a jObj
        | None -> jObj

    let readWith (decode: JsonReader<'a>) (k: string) (jsonObject: JsonObject) : Result<'a,JsonFailure list> =
        find k jsonObject
        |> JsonResult.bind (JsonResult.withPropertyTag k decode)

    let readOptionalWith (decode: JsonReader<'a>) (k: string) (jsonObject: JsonObject) : Result<'a option,JsonFailure list> =
        tryFind k jsonObject
        |> Option.map (JsonResult.withPropertyTag k decode)
        |> function
            | Some (Ok a) -> Ok (Some a)
            | Some (Error e) -> Error e
            | None -> Ok None

    let readChildWith (decode: ObjectReader<'a>) (k: string) (jsonObject: JsonObject) =
        readWith (ObjectReader.toJsonReader decode) k jsonObject

    let readOptionalChildWith (decode: ObjectReader<'a>) (k: string) (jsonObject: JsonObject) =
        readOptionalWith (ObjectReader.toJsonReader decode) k jsonObject

    let encode jObj = Json.Object jObj
    let decode json = Internal.jsonToJsonObject json

    let propertyFolder (encode: JsonWriter<'a>) s (k,v) = (k, encode v) :: s

    let toPropertyList = function
        | WriteObject ps -> ps
        | ReadObject (ps, _) -> ps

    let ofPropertyList (ps: (string * Json) list): JsonObject =
        List.rev ps
        |> JsonObject.WriteObject
    let ofPropertyListWith (encode: JsonWriter<'a>) (ps: (string * 'a) list): JsonObject =
        List.fold (propertyFolder encode) [] ps
        |> JsonObject.WriteObject

    let toMap = function
        | WriteObject ps -> listToMap ps
        | ReadObject (_, mps) -> mps

    let ofMap m = ReadObject (mapToList m, m)
    let ofMapWith (encode: JsonWriter<'a>) (m: Map<string, 'a>): JsonObject=
        let newMap = Map.map (fun _ a -> encode a) m
        ReadObject (mapToList newMap, newMap)
    let ofMapWithCustomKey (toString: 'k -> string) (encode: JsonWriter<'v>) (m: Map<'k, 'v>): JsonObject=
        let newList =
            mapToList m
            |> List.map (fun (k,v) -> (toString k, encode v))
        ReadObject (newList, Map.ofList newList)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Json =
    let Null: Json = Json.Null

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
            | Success (json, _, _) -> JsonResult.ok json
            | Failure (e, _, _) -> JsonResult.parserFailure e

    module Json =
        let parse s =
            if System.String.IsNullOrWhiteSpace s then
                JsonResult.noInput
            else
                Parser.parseJsonString s
                |> Parser.handleParserResult

        let import s =
            fun json ->
                match parse s with
                | Ok json' -> Ok (), json'
                | Error e -> Error e, json

        let parseStream (stream : #System.IO.Stream) =
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

        let addNameSeparator sb { PropertyNameSpacing = pns } =
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
            addNameSeparator sb options
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

[<AutoOpen>]
module Serialization =
    module Json =
        module Encode =
            let json json: Json = json

            let jsonObject (jObj: JsonObject): Json =
                JsonObject.encode jObj

            let jsonObjectWith (encode: ObjectWriter<'a>) (a: 'a): Json =
                JsonObject.buildWith encode a

            let propertyList (ps: (string * Json) list): Json =
                JsonObject.ofPropertyList ps
                |> jsonObject

            let propertyListWith (encode: JsonWriter<'a>) (ps: (string * 'a) list): Json =
                JsonObject.ofPropertyListWith encode ps
                |> jsonObject

            let list (els: Json list): Json =
                Json.Array els

            let listWith (encode: JsonWriter<'a>) (els: 'a list): Json =
                List.map encode els
                |> list

            let array (els: Json array): Json =
                List.ofArray els
                |> list

            let arrayWith (encode: JsonWriter<'a>) (els: 'a array): Json =
                Array.map encode els
                |> array

            let option (jO: Json option): Json =
                Option.defaultValue Null jO

            let optionWith (encode: JsonWriter<'a>) (aO: 'a option): Json =
                Option.map encode aO
                |> option

            // let set (els: Set<Json>): Json =
            //     Set.toList els
            //     |> list

            let setWith (encode: JsonWriter<'a>) (els: Set<'a>): Json =
                Set.toList els
                |> listWith encode

            let map (m: Map<string, Json>): Json =
                JsonObject.ofMap m
                |> jsonObject

            let mapWith (encode: JsonWriter<'a>) (m: Map<string, 'a>): Json =
                JsonObject.ofMapWith encode m
                |> jsonObject

            let mapWithCustomKey (toString: 'a -> string) (encode: JsonWriter<'b>) (m: Map<'a, 'b>): Json =
                JsonObject.ofMapWithCustomKey toString encode m
                |> jsonObject

            let tuple2 (encodeA: JsonWriter<'a>) (encodeB: JsonWriter<'b>) ((a : 'a), (b : 'b)): Json =
                list
                    [ encodeA a
                      encodeB b ]

            let tuple3 (encodeA: JsonWriter<'a>) (encodeB: JsonWriter<'b>) (encodeC: JsonWriter<'c>) ((a : 'a), (b : 'b), (c: 'c)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c ]

            let tuple4 (encodeA: JsonWriter<'a>) (encodeB: JsonWriter<'b>) (encodeC: JsonWriter<'c>) (encodeD: JsonWriter<'d>) ((a : 'a), (b : 'b), (c: 'c), (d: 'd)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c
                      encodeD d ]

            let tuple5 (encodeA: JsonWriter<'a>) (encodeB: JsonWriter<'b>) (encodeC: JsonWriter<'c>) (encodeD: JsonWriter<'d>) (encodeE: JsonWriter<'e>) ((a : 'a), (b : 'b), (c: 'c), (d: 'd), (e: 'e)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c
                      encodeD d
                      encodeE e ]

            let tuple6 (encodeA: JsonWriter<'a>) (encodeB: JsonWriter<'b>) (encodeC: JsonWriter<'c>) (encodeD: JsonWriter<'d>) (encodeE: JsonWriter<'e>) (encodeF: JsonWriter<'f>) ((a : 'a), (b : 'b), (c: 'c), (d: 'd), (e: 'e), (f: 'f)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c
                      encodeD d
                      encodeE e
                      encodeF f ]

            let tuple7 (encodeA: JsonWriter<'a>) (encodeB: JsonWriter<'b>) (encodeC: JsonWriter<'c>) (encodeD: JsonWriter<'d>) (encodeE: JsonWriter<'e>) (encodeF: JsonWriter<'f>) (encodeG: JsonWriter<'g>) ((a : 'a), (b : 'b), (c: 'c), (d: 'd), (e: 'e), (f: 'f), (g: 'g)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c
                      encodeD d
                      encodeE e
                      encodeF f
                      encodeG g ]

            let string (str: string): Json =
                Json.String str

            let number (n: string): Json =
                Json.Number n

            let int16 (n: int16): Json =
                n.ToString()
                |> number

            let int (n: int): Json =
                n.ToString()
                |> number

            let int64 (n: int64): Json =
                n.ToString()
                |> number

            let uint16 (n: uint16): Json =
                n.ToString()
                |> number

            let uint32 (n: uint32): Json =
                n.ToString()
                |> number

            let uint64 (n: uint64): Json =
                n.ToString()
                |> number

            let single (n: single): Json =
                n.ToString("R")
                |> number

            let float (n: float): Json =
                n.ToString("G17")
                |> number

            let decimal (n: decimal): Json =
                n.ToString()
                |> number

            let bigint (n: bigint): Json =
                n.ToString("R")
                |> number

            let dateTime (dt: System.DateTime): Json =
                dt.ToUniversalTime().ToString("o")
                |> string

            let dateTimeOffset (dt: System.DateTimeOffset): Json =
                dt.ToString("o")
                |> string

            let guid (g: System.Guid): Json =
                g.ToString("N")
                |> string

            let bytes (bs: byte array): Json =
                System.Convert.ToBase64String bs
                |> string

            let bool (b: bool): Json =
                if b then Json.True else Json.False

            let unit () : Json =
                Null

        module Decode =
            let json (json: Json) = Ok json

            let unit (json: Json) =
                match json with
                | Json.Null -> Ok ()
                | _ -> JsonResult.typeMismatch JsonMemberType.Null json

            let oneOf (decoders: JsonReader<'a> list) (json: Json) =
                let rec inner decoders s i =
                    match s, decoders with
                    | _, [] | Ok _, _ -> s
                    | Error oldErrs, decode :: decoders ->
                        let r = decode json
                        match r with
                        | Ok _ -> r
                        | Error errs ->
                            inner decoders (Error (oldErrs @ (JsonFailure.tagList (ChoiceTag i) errs))) (i + 1u)
                inner decoders JsonResult.emptyError 0u

            let jsonObject (json: Json) =
                JsonObject.decode json

            let jsonObjectWith (decode: ObjectReader<'a>) (json: Json) =
                jsonObject json
                |> JsonResult.bind decode

            let list (json: Json) =
                match json with
                | Json.Array a -> Ok a
                | _ -> JsonResult.typeMismatch JsonMemberType.Array json

            let listInnerQuick (decode: JsonReader<'a>) (init: 's) (fold: 'a -> 's -> 's) (xs: Json list) : JsonResult<'s> =
                let folder json (sR, i) =
                    let next =
                        match sR with
                        | Ok s ->
                            let aR = JsonResult.withIndexTag i decode json
                            match aR with
                            | Ok a -> Ok (fold a s)
                            | Error e -> Error e
                        | Error e -> Error e
                    (next, i + 1u)
                List.foldBack folder xs (Ok init, 0u)
                |> fst

            let listInner  (decode: JsonReader<'a>) (init: 's) (fold: 'a -> 's -> 's) (xs: Json list): JsonResult<'s>=
                let folder json (sR, i) =
                    let aR = JsonResult.withIndexTag i decode json
                    let next =
                        match sR, aR with
                        | Ok s, Ok a -> Ok (fold a s)
                        | Error errs, Error [err] -> Error (err :: errs)
                        | Error err1, Error err2 -> Error (err2 @ err1)
                        | Error errs, _
                        | _, Error errs -> Error errs
                    (next, i + 1u)
                List.foldBack folder xs (Ok init, 0u)
                |> fst

            let listFolder<'a> : 'a -> 'a list -> 'a list =
                do ()
                fun x xs -> x :: xs

            let listWithQuick (decode: JsonReader<'a>) (json: Json) : JsonResult<'a list> =
                list json
                |> JsonResult.bind (listInnerQuick decode [] listFolder)

            let listWith (decode: JsonReader<'a>) (json: Json) : JsonResult<'a list> =
                list json
                |> JsonResult.bind (listInner decode [] listFolder)

            let array (json: Json) =
                list json
                |> JsonResult.map Array.ofList

            let arrayFolder<'a> : 'a -> 'a array * int -> 'a array * int =
                do ()
                fun x (arr: 'a array, idx) ->
                    arr.[idx] <- x
                    (arr, idx - 1)

            let arrayWithQuick (decode: JsonReader<'a>) (json: Json) =
                list json
                |> JsonResult.bind (fun jLst -> let len = List.length jLst in listInnerQuick decode (Array.zeroCreate len, len - 1) arrayFolder jLst)
                |> JsonResult.map fst

            let arrayWith (decode: JsonReader<'a>) (json: Json) =
                list json
                |> JsonResult.bind (fun jLst -> let len = List.length jLst in listInner decode (Array.zeroCreate len, len - 1) arrayFolder jLst)
                |> JsonResult.map fst

            let optionWith (decode: JsonReader<'a>) =
                let decode =
                    oneOf
                        [ unit |> JsonReader.map (fun () -> None)
                          decode |> JsonReader.map Some ]
                fun json -> decode json

            let option =
                optionWith json

            // let set (json: Json) =
            //     list json
            //     |> JsonResult.map Set.ofList

            let setFolder = Set.add

            let setWithQuick (decode: JsonReader<'a>) (json: Json) =
                list json
                |> JsonResult.bind (listInnerQuick decode Set.empty setFolder)

            let setWith (decode: JsonReader<'a>) (json: Json) =
                list json
                |> JsonResult.bind (listInner decode Set.empty setFolder)

            let map (json: Json) =
                jsonObject json
                |> JsonResult.map JsonObject.toMap

            let deserKeyErr<'a> e = JsonFailure.DeserializationError (typeof<'a>, "Unable to parse key:" + e)
            let mapInnerQuick (parseKey : string -> Result<'k,string>) (decode: JsonReader<'v>) (kvps: (string * Json) list): JsonResult<Map<'k,'v>> =
                List.fold (fun sR (kStr,json) ->
                    match sR with
                    | Ok s ->
                        let kR = parseKey kStr
                        match kR with
                        | Ok k ->
                            let vR = JsonResult.withPropertyTag kStr decode json
                            match vR with
                            | Ok v -> Ok (Map.add k v s)
                            | Error e -> Error e
                        | Error e -> Error [deserKeyErr<'k> e]
                    | Error e -> Error e) (Ok Map.empty) kvps

            let mapInner (parseKey : string -> Result<'k,string>) (decode: JsonReader<'v>) (kvps: (string * Json) list): JsonResult<Map<'k,'v>> =
                List.fold (fun sR (k,json) ->
                    let kR = parseKey k
                    let vR = JsonResult.withPropertyTag k decode json
                    match sR, kR, vR with
                    | Ok s, Ok k, Ok v -> Ok (Map.add k v s)
                    | Error errs, Error errK, Error [errV] -> Error (errV :: (deserKeyErr<'k> errK) :: errs)
                    | Error errs, Error err, Ok _ -> Error (deserKeyErr<'k> err :: errs)
                    | Error errs, Ok _, Error [err] -> Error (err :: errs)
                    | Error errs, Error errK, Error errV -> Error (errV @ (deserKeyErr<'k> errK :: errs))
                    | Error errs, _, _
                    | _, _, Error errs -> Error errs
                    | _, Error err, _ -> Error [deserKeyErr<'k> err]) (Ok Map.empty) kvps

            let mapWithQuick (decode: JsonReader<'a>) (json: Json) =
                jsonObject json
                |> JsonResult.bind (fun jObj ->
                    let xs = JsonObject.toPropertyList jObj
                    mapInnerQuick Ok decode xs)

            let mapWith (decode: JsonReader<'a>) (json: Json) =
                jsonObject json
                |> JsonResult.bind (fun jObj ->
                    let xs = JsonObject.toPropertyList jObj
                    mapInner Ok decode xs)

            let mapWithCustomKeyQuick (parseKey: string -> Result<'a,string>) (decode: JsonReader<'b>) (json: Json) =
                jsonObject json
                |> JsonResult.bind (fun jObj ->
                    let xs = JsonObject.toPropertyList jObj
                    mapInnerQuick parseKey decode xs)

            let mapWithCustomKey (parseKey: string -> Result<'a,string>) (decode: JsonReader<'b>) (json: Json) =
                jsonObject json
                |> JsonResult.bind (fun jObj ->
                    let xs = JsonObject.toPropertyList jObj
                    mapInner parseKey decode xs)

            let tuple2 (json: Json) =
                list json
                |> JsonResult.bind (function
                    | [a;b] -> Ok (a, b)
                    | [_] -> JsonResult.deserializationError "2-Tuple has only one element"
                    | [] -> JsonResult.deserializationError "2-Tuple has zero elements"
                    | _ -> JsonResult.deserializationError "2-Tuple has too many elements")

            let mk2Tuple = Ok (fun a b -> (a, b))
            let tuple2WithQuick (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (json: Json) =
                tuple2 json
                |> JsonResult.bind (fun (a, b) ->
                    mk2Tuple
                    |> JsonResult.applyShort (JsonResult.withIndexTag 0u decodeA) a
                    |> JsonResult.applyShort (JsonResult.withIndexTag 1u decodeB) b)

            let tuple2With (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (json: Json) =
                tuple2 json
                |> JsonResult.bind (fun (a, b) ->
                    mk2Tuple
                    |> JsonResult.apply (JsonResult.withIndexTag 0u decodeA a)
                    |> JsonResult.apply (JsonResult.withIndexTag 1u decodeB b))

            let tuple3 (json: Json) =
                list json
                |> JsonResult.bind (function
                    | [a;b;c] -> Ok (a, b, c)
                    | x when List.length x > 3 -> JsonResult.deserializationError "3-Tuple has excess elements"
                    | _ -> JsonResult.deserializationError "3-Tuple has insufficient elements")

            let mk3Tuple = Ok (fun a b c -> (a, b, c))
            let tuple3WithQuick (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (json: Json) =
                tuple3 json
                |> JsonResult.bind (fun (a, b, c) ->
                    mk3Tuple
                    |> JsonResult.applyShort (JsonResult.withIndexTag 0u decodeA) a
                    |> JsonResult.applyShort (JsonResult.withIndexTag 1u decodeB) b
                    |> JsonResult.applyShort (JsonResult.withIndexTag 2u decodeC) c)

            let tuple3With (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (json: Json) =
                tuple3 json
                |> JsonResult.bind (fun (a, b, c) ->
                    mk3Tuple
                    |> JsonResult.apply (JsonResult.withIndexTag 0u decodeA a)
                    |> JsonResult.apply (JsonResult.withIndexTag 1u decodeB b)
                    |> JsonResult.apply (JsonResult.withIndexTag 2u decodeC c))

            let tuple4 (json: Json) =
                list json
                |> JsonResult.bind (function
                    | [a;b;c;d] -> Ok (a, b, c, d)
                    | x when List.length x > 4 -> JsonResult.deserializationError "4-Tuple has excess elements"
                    | _ -> JsonResult.deserializationError "4-Tuple has insufficient elements")

            let mk4Tuple = Ok (fun a b c d -> (a, b, c, d))
            let tuple4WithQuick (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (decodeD: JsonReader<'d>) (json: Json) =
                tuple4 json
                |> JsonResult.bind (fun (a, b, c, d) ->
                    mk4Tuple
                    |> JsonResult.applyShort (JsonResult.withIndexTag 0u decodeA) a
                    |> JsonResult.applyShort (JsonResult.withIndexTag 1u decodeB) b
                    |> JsonResult.applyShort (JsonResult.withIndexTag 2u decodeC) c
                    |> JsonResult.applyShort (JsonResult.withIndexTag 3u decodeD) d)

            let tuple4With (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (decodeD: JsonReader<'d>) (json: Json) =
                tuple4 json
                |> JsonResult.bind (fun (a, b, c, d) ->
                    mk4Tuple
                    |> JsonResult.apply (JsonResult.withIndexTag 0u decodeA a)
                    |> JsonResult.apply (JsonResult.withIndexTag 1u decodeB b)
                    |> JsonResult.apply (JsonResult.withIndexTag 2u decodeC c)
                    |> JsonResult.apply (JsonResult.withIndexTag 3u decodeD d))

            let tuple5 (json: Json) =
                list json
                |> JsonResult.bind (function
                    | [a;b;c;d;e] -> Ok (a, b, c, d, e)
                    | x when List.length x > 5 -> JsonResult.deserializationError "5-Tuple has excess elements"
                    | _ -> JsonResult.deserializationError "5-Tuple has insufficient elements")

            let mk5Tuple = Ok (fun a b c d e -> (a, b, c, d, e))
            let tuple5WithQuick (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (decodeD: JsonReader<'d>) (decodeE: JsonReader<'e>) (json: Json) =
                tuple5 json
                |> JsonResult.bind (fun (a, b, c, d, e) ->
                    mk5Tuple
                    |> JsonResult.applyShort (JsonResult.withIndexTag 0u decodeA) a
                    |> JsonResult.applyShort (JsonResult.withIndexTag 1u decodeB) b
                    |> JsonResult.applyShort (JsonResult.withIndexTag 2u decodeC) c
                    |> JsonResult.applyShort (JsonResult.withIndexTag 3u decodeD) d
                    |> JsonResult.applyShort (JsonResult.withIndexTag 4u decodeE) e)

            let tuple5With (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (decodeD: JsonReader<'d>) (decodeE: JsonReader<'e>) (json: Json) =
                tuple5 json
                |> JsonResult.bind (fun (a, b, c, d, e) ->
                    mk5Tuple
                    |> JsonResult.apply (JsonResult.withIndexTag 0u decodeA a)
                    |> JsonResult.apply (JsonResult.withIndexTag 1u decodeB b)
                    |> JsonResult.apply (JsonResult.withIndexTag 2u decodeC c)
                    |> JsonResult.apply (JsonResult.withIndexTag 3u decodeD d)
                    |> JsonResult.apply (JsonResult.withIndexTag 4u decodeE e))

            let tuple6 (json: Json) =
                list json
                |> JsonResult.bind (function
                    | [a;b;c;d;e;f] -> Ok (a, b, c, d, e, f)
                    | x when List.length x > 6 -> JsonResult.deserializationError "6-Tuple has excess elements"
                    | _ -> JsonResult.deserializationError "6-Tuple has insufficient elements")

            let mk6Tuple = Ok (fun a b c d e f -> (a, b, c, d, e, f))
            let tuple6WithQuick (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (decodeD: JsonReader<'d>) (decodeE: JsonReader<'e>) (decodeF: JsonReader<'f>) (json: Json) =
                tuple6 json
                |> JsonResult.bind (fun (a, b, c, d, e, f) ->
                    mk6Tuple
                    |> JsonResult.applyShort (JsonResult.withIndexTag 0u decodeA) a
                    |> JsonResult.applyShort (JsonResult.withIndexTag 1u decodeB) b
                    |> JsonResult.applyShort (JsonResult.withIndexTag 2u decodeC) c
                    |> JsonResult.applyShort (JsonResult.withIndexTag 3u decodeD) d
                    |> JsonResult.applyShort (JsonResult.withIndexTag 4u decodeE) e
                    |> JsonResult.applyShort (JsonResult.withIndexTag 5u decodeF) f)

            let tuple6With (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (decodeD: JsonReader<'d>) (decodeE: JsonReader<'e>) (decodeF: JsonReader<'f>) (json: Json) =
                tuple6 json
                |> JsonResult.bind (fun (a, b, c, d, e, f) ->
                    mk6Tuple
                    |> JsonResult.apply (JsonResult.withIndexTag 0u decodeA a)
                    |> JsonResult.apply (JsonResult.withIndexTag 1u decodeB b)
                    |> JsonResult.apply (JsonResult.withIndexTag 2u decodeC c)
                    |> JsonResult.apply (JsonResult.withIndexTag 3u decodeD d)
                    |> JsonResult.apply (JsonResult.withIndexTag 4u decodeE e)
                    |> JsonResult.apply (JsonResult.withIndexTag 5u decodeF f))

            let tuple7 (json: Json) =
                list json
                |> JsonResult.bind (function
                    | [a;b;c;d;e;f;g] -> Ok (a, b, c, d, e, f, g)
                    | x when List.length x > 7 -> JsonResult.deserializationError "7-Tuple has excess elements"
                    | _ -> JsonResult.deserializationError "7-Tuple has insufficient elements")

            let mk7Tuple = Ok (fun a b c d e f g -> (a, b, c, d, e, f, g))
            let tuple7WithQuick (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (decodeD: JsonReader<'d>) (decodeE: JsonReader<'e>) (decodeF: JsonReader<'f>) (decodeG: JsonReader<'g>) (json: Json) =
                tuple7 json
                |> JsonResult.bind (fun (a, b, c, d, e, f, g) ->
                    mk7Tuple
                    |> JsonResult.applyShort (JsonResult.withIndexTag 0u decodeA) a
                    |> JsonResult.applyShort (JsonResult.withIndexTag 1u decodeB) b
                    |> JsonResult.applyShort (JsonResult.withIndexTag 2u decodeC) c
                    |> JsonResult.applyShort (JsonResult.withIndexTag 3u decodeD) d
                    |> JsonResult.applyShort (JsonResult.withIndexTag 4u decodeE) e
                    |> JsonResult.applyShort (JsonResult.withIndexTag 5u decodeF) f
                    |> JsonResult.applyShort (JsonResult.withIndexTag 6u decodeG) g)

            let tuple7With (decodeA: JsonReader<'a>) (decodeB: JsonReader<'b>) (decodeC: JsonReader<'c>) (decodeD: JsonReader<'d>) (decodeE: JsonReader<'e>) (decodeF: JsonReader<'f>) (decodeG: JsonReader<'g>) (json: Json) =
                tuple7 json
                |> JsonResult.bind (fun (a, b, c, d, e, f, g) ->
                    mk7Tuple
                    |> JsonResult.apply (JsonResult.withIndexTag 0u decodeA a)
                    |> JsonResult.apply (JsonResult.withIndexTag 1u decodeB b)
                    |> JsonResult.apply (JsonResult.withIndexTag 2u decodeC c)
                    |> JsonResult.apply (JsonResult.withIndexTag 3u decodeD d)
                    |> JsonResult.apply (JsonResult.withIndexTag 4u decodeE e)
                    |> JsonResult.apply (JsonResult.withIndexTag 5u decodeF f)
                    |> JsonResult.apply (JsonResult.withIndexTag 6u decodeG g))

            let number (json: Json) =
                match json with
                | Json.Number n -> Ok n
                | _ -> JsonResult.typeMismatch JsonMemberType.Number json

            let int16 (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Int16.Parse)

            let int (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Int32.Parse)

            let int64 (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Int64.Parse)

            let uint16 (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.UInt16.Parse)

            let uint32 (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.UInt32.Parse)

            let uint64 (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.UInt64.Parse)

            let single (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Single.Parse)

            let float (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Double.Parse)

            let decimal (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Decimal.Parse)

            let bigint (json: Json) =
                number json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Numerics.BigInteger.Parse)

            let string (json: Json) =
                match json with
                | Json.String s -> Ok s
                | _ -> JsonResult.typeMismatch JsonMemberType.String json

            let dateTimeParser s = System.DateTime.ParseExact (s, [| "s"; "r"; "o" |], null, System.Globalization.DateTimeStyles.AdjustToUniversal)
            let dateTime (json: Json) =
                string json
                |> JsonResult.bind (JsonResult.fromThrowingConverter dateTimeParser)

            let dateTimeOffsetParser s = System.DateTimeOffset.ParseExact (s, [| "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF'Z'"; "o"; "r" |], null, System.Globalization.DateTimeStyles.AssumeUniversal)
            let dateTimeOffset (json: Json) =
                string json
                |> JsonResult.bind (JsonResult.fromThrowingConverter dateTimeOffsetParser)

            let guid (json: Json) =
                string json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Guid.Parse)

            let bytes (json: Json) =
                string json
                |> JsonResult.bind (JsonResult.fromThrowingConverter System.Convert.FromBase64String)

            let bool (json: Json) =
                match json with
                | Json.True -> Ok true
                | Json.False -> Ok false
                | _ -> JsonResult.typeMismatch JsonMemberType.Bool json

        let deserializeWith (decode: JsonReader<'a>) (str: string) =
            Json.parse str
            |> JsonResult.bind decode

        let deserializeObjectWith (decode: ObjectReader<'a>) (str: string) =
            Json.parse str
            |> JsonResult.bind (Decode.jsonObject)
            |> JsonResult.bind decode

        let serializeWith (encode: JsonWriter<'a>) (options: JsonFormattingOptions) (a: 'a) =
            encode a
            |> Json.formatWith options

        let serializeObjectWith (encode: ObjectWriter<'a>) (options: JsonFormattingOptions) (a: 'a) =
            JsonObject.buildWith encode a
            |> Json.formatWith options

module Optics =
    type Lens<'a,'b> = ('a -> JsonResult<'b>) * ('b -> 'a -> 'a)

    let get (l:Lens<'a,'b>) =
        fst l

    let set (l:Lens<'a,'b>) =
        snd l

    let compose (l1:Lens<'a,'b>) (l2:Lens<'b,'c>): Lens<'a,'c> =
        (fun a -> fst l1 a |> JsonResult.bind (fst l2)), (fun c a -> match JsonResult.map (snd l2 c) (fst l1 a) with Ok b -> snd l1 b a; | _ -> a)

    module JsonObject =
        let key_ k =
            JsonObject.find k, fun v jObj -> JsonObject.add k v jObj

    module Json =
        module E = Serialization.Json.Encode
        module D = Serialization.Json.Decode

        let Object__ = D.jsonObject, E.jsonObject
        let Array__ = D.list, E.array
        let String__ = D.string, E.string
        let Number__ = D.number, E.number
        let Bool__ = D.bool, E.bool
        let Null__ = D.unit, E.unit

        let Object_ = D.jsonObject, fun x _ -> E.jsonObject x
        let Array_ = D.list, fun x _ -> E.list x
        let String_ = D.string, fun x _ -> E.string x
        let Number_ = D.number, fun x _ -> E.number x
        let Bool_ = D.bool, fun x _ -> E.bool x
        let Null_ = D.unit, fun () _ -> E.unit ()
        let Int16_ : Lens<Json,int16> =
            D.int16, fun x _ -> E.int16 x

        let Int32_ : Lens<Json,int> =
            D.int, fun x _ -> E.int x

        let Int64_ : Lens<Json,int64> =
            D.int64, fun x _ -> E.int64 x

        let UInt16_ : Lens<Json,uint16> =
            D.uint16, fun x _ -> E.uint16 x

        let UInt32_ : Lens<Json,uint32> =
            D.uint32, fun x _ -> E.uint32 x

        let UInt64_ : Lens<Json,uint64> =
            D.uint64, fun x _ -> E.uint64 x

        let Single_ : Lens<Json,single> =
            D.single, fun x _ -> E.single x

        let Double_ : Lens<Json,float> =
            D.float, fun x _ -> E.float x

        let Decimal_ : Lens<Json,decimal> =
            D.decimal, fun x _ -> E.decimal x

        let BigInteger_ : Lens<Json,bigint> =
            D.bigint, fun x _ -> E.bigint x

        let DateTime_ : Lens<Json,System.DateTime> =
            D.dateTime, fun x _ -> E.dateTime x

        let DateTimeOffset_ : Lens<Json,System.DateTimeOffset> =
            D.dateTimeOffset, fun x _ -> E.dateTimeOffset x

        let Guid_ : Lens<Json,System.Guid> =
            D.guid, fun x _ -> E.guid x

        let Bytes_ : Lens<Json,byte array> =
            D.bytes, fun x _ -> E.bytes x

        let Property_ k =
            compose Object_ (JsonObject.key_ k)

module JsonTransformer =
    type Json<'a> = Json -> JsonResult<'a> * Json

    module Json =
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

        let toJsonReader (aJ:Json<'a>): JsonReader<'a> =
            fun json ->
                aJ json |> fst

    module Operators =
        let inline (>>=) m f = Json.bind f m
        let inline (=<<) f m = Json.bind f m
        let inline (<*>) f m = Json.apply m f
        let inline (<!>) f m = Json.map f m
        let inline ( *>) m1 m2 = Json.map2 (fun _ x -> x) m1 m2
        let inline ( <*) m1 m2 = Json.map2 (fun x _ -> x) m1 m2
        let (>=>) m1 m2 = m1 >> Json.bind m2
        let (<=<) m2 m1 = m1 >> Json.bind m2

#nowarn "60"
type Json with
    override x.ToString() =
        Formatting.Json.format x

module Inference =
    module Internal =
        module E = Serialization.Json.Encode
        module D = Serialization.Json.Decode

        type ChironDefaults = ChironDefaults with
            static member ToJson (jObj: JsonObject): Json = E.jsonObject jObj
            static member ToJson (str: string): Json = E.string str
            static member ToJson (n: int16): Json = E.int16 n
            static member ToJson (n: int): Json = E.int n
            static member ToJson (n: int64): Json = E.int64 n
            static member ToJson (n: uint16): Json = E.uint16 n
            static member ToJson (n: uint32): Json = E.uint32 n
            static member ToJson (n: uint64): Json = E.uint64 n
            static member ToJson (n: single): Json = E.single n
            static member ToJson (n: float): Json = E.float n
            static member ToJson (n: decimal): Json = E.decimal n
            static member ToJson (n: bigint): Json = E.bigint n
            static member ToJson (b: bool): Json = E.bool b
            static member ToJson (dt: System.DateTime): Json = E.dateTime dt
            static member ToJson (dt: System.DateTimeOffset): Json = E.dateTimeOffset dt
            static member ToJson (g: System.Guid): Json = E.guid g
            static member ToJson (j: Json): Json = E.json j

            static member FromJson (_: JsonObject, json: Json) = D.jsonObject json
            static member FromJson (_: string, json: Json) = D.string json
            static member FromJson (_: int16, json: Json) = D.int16 json
            static member FromJson (_: int, json: Json) = D.int json
            static member FromJson (_: int64, json: Json) = D.int64 json
            static member FromJson (_: uint16, json: Json) = D.uint16 json
            static member FromJson (_: uint32, json: Json) = D.uint32 json
            static member FromJson (_: uint64, json: Json) = D.uint64 json
            static member FromJson (_: single, json: Json) = D.single json
            static member FromJson (_: float, json: Json) = D.float json
            static member FromJson (_: decimal, json: Json) = D.decimal json
            static member FromJson (_: bigint, json: Json) = D.bigint json
            static member FromJson (_: bool, json: Json) = D.bool json
            static member FromJson (_: System.DateTime, json: Json) = D.dateTime json
            static member FromJson (_: System.DateTimeOffset, json: Json) = D.dateTimeOffset json
            static member FromJson (_: System.Guid, json: Json) = D.guid json
            static member FromJson (_: Json, json: Json) = D.json json

        let inline encodeWithDefaults (defaults: ^def) (a: ^a): Json =
            ((^a or ^def) : (static member ToJson : ^a -> Json) a)

        let inline encode (a: 'a) =
            encodeWithDefaults ChironDefaults a

        let inline decodeWithDefaults (defaults: ^def) (dummy: ^a) (json: Json): JsonResult<'a> =
            ((^a or ^def) : (static member FromJson : ^a * Json -> JsonResult<'a>) (dummy, json))

        let inline decode (json: Json) =
            decodeWithDefaults ChironDefaults Unchecked.defaultof<'a> json

        type ChironDefaults with
            static member inline ToJson (xs: 'a list): Json = E.listWith encode xs
            static member inline ToJson (xs: 'a array): Json = E.arrayWith encode xs
            static member inline ToJson (xO: 'a option): Json = E.optionWith encode xO
            static member inline ToJson (xs: Set<'a>): Json = E.setWith encode xs
            static member inline ToJson (m: Map<string, 'a>): Json = E.mapWith encode m
            static member inline ToJson (t): Json = E.tuple2 encode encode t
            static member inline ToJson (t): Json = E.tuple3 encode encode encode t
            static member inline ToJson (t): Json = E.tuple4 encode encode encode encode t
            static member inline ToJson (t): Json = E.tuple5 encode encode encode encode encode t
            static member inline ToJson (t): Json = E.tuple6 encode encode encode encode encode encode t
            static member inline ToJson (t): Json = E.tuple7 encode encode encode encode encode encode encode t

            static member inline FromJson (_: 'a list, json: Json) = D.listWith decode json
            static member inline FromJson (_: 'a array, json: Json): JsonResult<'a array> = D.arrayWith decode json
            static member inline FromJson (_: 'a option, json: Json) = D.optionWith decode json
            static member inline FromJson (_: Set<'a>, json: Json) = D.setWith decode json
            static member inline FromJson (_: Map<string, 'a>, json: Json) = D.mapWith decode json
            static member inline FromJson (_: 'a * 'b, json: Json) = D.tuple2With decode decode json
            static member inline FromJson (_: 'a * 'b * 'c, json: Json) = D.tuple3With decode decode decode json
            static member inline FromJson (_: 'a * 'b * 'c * 'd, json: Json) = D.tuple4With decode decode decode decode json
            static member inline FromJson (_: 'a * 'b * 'c * 'd * 'e, json: Json) = D.tuple5With decode decode decode decode decode json
            static member inline FromJson (_: 'a * 'b * 'c * 'd * 'e * 'f, json: Json) = D.tuple6With decode decode decode decode decode decode json
            static member inline FromJson (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g, json: Json) = D.tuple7With decode decode decode decode decode decode decode json

    module Json =
        let inline decodeWithDefaults (defaults: ^def) (a: ^a) (json: Json): JsonResult<'a> =
            Internal.decodeWithDefaults defaults a json

        let inline decode (json: Json) =
            Internal.decode json

        let inline decodeObject (json: Json): JsonResult<'a> =
            let decode jObj = (^a : (static member Decode: JsonObject -> JsonResult<'a>) jObj)
            ObjectReader.toJsonReader decode json

        let inline encodeWithDefaults (defaults: ^def) (a: ^a): Json =
            Internal.encodeWithDefaults defaults a

        let inline encode (a: 'a) =
            Internal.encode a

        let inline encodeObject (a: 'a): Json =
            (^a : (static member Encode: ^a * JsonObject -> JsonObject) (a, JsonObject.empty))
            |> encode

        let inline deserialize (str: string) =
            Json.parse str
            |> JsonResult.bind decode

        let inline deserializeObject (str: string) =
            Json.parse str
            |> JsonResult.bind decodeObject

        let inline serialize (a: 'a) =
            encode a
            |> Json.format

        let inline serializeObject (a: 'a) =
            encodeObject a
            |> Json.format

    module JsonObject =
        let inline writeWithDefaults (defaults : ^def) (k : string) (v : ^a) (jsonObject : JsonObject) =
            JsonObject.add k (Internal.encodeWithDefaults defaults v) jsonObject

        let inline write k v (jsonObject : JsonObject) =
            JsonObject.add k (Internal.encode v) jsonObject

        let inline writeOptionalWithDefaults (defaults : ^def) (k : string) (vO : ^a option) (jsonObject : JsonObject) =
            match vO with
            | Some v -> writeWithDefaults defaults k v jsonObject
            | None -> jsonObject

        let inline writeOptional k vO (jsonObject : JsonObject) =
            match vO with
            | Some v -> write k v jsonObject
            | None -> jsonObject

        let inline writeChild (k : string) (v : ^a) (jsonObject : JsonObject) =
            JsonObject.add k (Json.encodeObject v) jsonObject

        let inline writeOptionalChild (k : string) (vO : ^a option) (jsonObject : JsonObject) =
            match vO with
            | Some v -> writeChild k v jsonObject
            | None -> jsonObject

        let inline writeMixin v jObj =
            (^a : (static member Encode: ^a * JsonObject -> JsonObject) (v, jObj))

        let inline writeOptionalMixin vO jObj =
            match vO with
            | Some v -> writeMixin v jObj
            | None -> jObj

        let inline readWithDefaults (defaults : ^def) (k : string) (jsonObject: JsonObject) : JsonResult<'a> =
            JsonObject.readWith Json.decode k jsonObject

        let inline read (k: string) (jsonObject: JsonObject) : JsonResult<'a> =
            readWithDefaults Internal.ChironDefaults k jsonObject

        let inline readOptionalWithDefaults (defaults : ^def) (k : string) (jsonObject: JsonObject) : JsonResult<'a option> =
            JsonObject.readOptionalWith Json.decode k jsonObject

        let inline readOptional (k: string) (jsonObject: JsonObject) : JsonResult<'a option> =
            readOptionalWithDefaults Internal.ChironDefaults k jsonObject

        let inline readChild (k: string) (jsonObject: JsonObject): JsonResult<'a> =
            let decode jsonObject = (^a : (static member Decode : JsonObject -> JsonResult<'a>) jsonObject)
            JsonObject.find k jsonObject
            |> JsonResult.bind (JsonResult.withPropertyTag k (ObjectReader.toJsonReader decode))

        let inline readOptionalChild (k: string) (jsonObject: JsonObject): JsonResult<'a option> =
            let decode jsonObject = (^a : (static member Decode : JsonObject -> JsonResult<'a>) jsonObject)
            JsonObject.tryFind k jsonObject
            |> Option.map (JsonResult.withPropertyTag k (ObjectReader.toJsonReader decode))
            |> function
                | Some (Ok a) -> Ok (Some a)
                | Some (Error e) -> Error e
                | None -> Ok None

        let inline readMixin (jsonObject: JsonObject): JsonResult<'a> =
            (^a : (static member Decode : JsonObject -> JsonResult<'a>) jsonObject)

[<AutoOpen>]
module Builders =
    open JsonTransformer

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
        member __.Delay(u2bM) : ObjectReader<'a> = ObjectReader.init () |> ObjectReader.bind u2bM

        member __.Run (aM : ObjectReader<'a>) : JsonReader<'a> =
            ObjectReader.toJsonReader aM

    let json = JsonBuilder ()
    let jsonReader = JsonReaderBuilder()

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
        Optics.get (Optics.Json.Property_ key) json
        |> JsonResult.bind r
        |> function
            | Result.Ok x -> Some x
            | _ -> None

    let inline (|Property|_|) (key: string) (json: Json): 'a option =
        Optics.get (Optics.Json.Property_ key) json
        |> JsonResult.bind Inference.Json.decode
        |> function
            | Result.Ok x -> Some x
            | _ -> None
