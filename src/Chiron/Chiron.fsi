namespace Chiron

type JsonMemberType =
    | Object
    | Array
    | String
    | Number
    | Bool
    | Null

type JsonObject

type Json

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

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonMemberType =
    val ofJson: Json -> JsonMemberType
    val describe: memberType: JsonMemberType -> string

module Optic =
    type Epi<'a,'b> = ('a -> JsonResult<'b>) * ('b -> 'a)
    type Lens<'a,'b> = ('a -> JsonResult<'b>) * ('b -> 'a -> 'a)

    val get: lens: Lens<'a,'b> -> ('a -> JsonResult<'b>)
    val set: lens: Lens<'a,'b> -> ('b -> 'a -> 'a)
    val compose: Lens<'a,'b> -> Lens<'b,'c> -> Lens<'a,'c>

[<AutoOpen>]
module Parsing =
    open FParsec

    module Parser =
        val jstring: Parser<Json,unit>
        val jnumber: Parser<Json,unit>
        val jtrue: Parser<Json,unit>
        val jfalse: Parser<Json,unit>
        val jnull: Parser<Json,unit>
        val jlist: Parser<Json,unit>
        val jobject: Parser<Json,unit>
        val json: Parser<Json,unit>

    [<RequireQualifiedAccess>]
    module Json =
        val tryParse: jsonStr: string -> JsonResult<Json>
        val parseOrThrow: jsonStr: string -> Json
        val import: jsonStr: string -> Json<unit>
        val tryParseStream: stream: #System.IO.Stream -> JsonResult<Json>

[<AutoOpen>]
module Formatting =
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
      static member Compact: JsonFormattingOptions
      static member SingleLine: JsonFormattingOptions
      static member Pretty: JsonFormattingOptions

    [<RequireQualifiedAccess>]
    module Json =
        val formatWith: opts: JsonFormattingOptions -> json: Json -> string
        val format: json: Json -> string

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Json =
    val ofJsonObject: JsonObject -> Json
    val ofList: Json list -> Json
    val ofListWith: toJson: JsonWriter<'a> -> 'a list -> Json
    val ofString: string -> Json
    val ofInt16: int16 -> Json
    val ofInt32: int -> Json
    val ofInt64: int64 -> Json
    val ofUInt16: uint16 -> Json
    val ofUInt32: uint32 -> Json
    val ofUInt64: uint64 -> Json
    val ofFloat: float -> Json
    val ofSingle: single -> Json
    val ofDecimal: decimal -> Json
    val ofBigInt: bigint -> Json
    val ofBool: bool -> Json
    val ofDateTime: System.DateTime -> Json
    val ofDateTimeOffset: System.DateTimeOffset -> Json
    val ofGuid: System.Guid -> Json
    val ofBytes: byte array -> Json
    val ``null``: Json

    module Inference =
        type ToJsonDefaults = ToJsonDefaults with
            static member ToJson: (string * Json) list -> Json
            static member ToJson: JsonObject -> Json
            static member ToJson: Json list -> Json
            static member ToJson: string -> Json
            static member ToJson: int16 -> Json
            static member ToJson: int -> Json
            static member ToJson: int64 -> Json
            static member ToJson: uint16 -> Json
            static member ToJson: uint32 -> Json
            static member ToJson: uint64 -> Json
            static member ToJson: float -> Json
            static member ToJson: single -> Json
            static member ToJson: decimal -> Json
            static member ToJson: bigint -> Json
            static member ToJson: byte array -> Json
            static member ToJson: bool -> Json
            static member ToJson: unit -> Json

    val inline inferWithDefaults: defaults: ^def -> JsonWriter<'a> when (^a or ^def) : (static member ToJson : 'a -> Json)
    val inline infer: a: 'a -> Json when (^a or Inference.ToJsonDefaults) : (static member ToJson : 'a -> Json)

    module Optics =
        val Object__: Optic.Epi<Json,JsonObject>
        val Array__: Optic.Epi<Json,Json list>
        val String__: Optic.Epi<Json,string>
        val Number__: Optic.Epi<Json,string>
        val Bool__: Optic.Epi<Json,bool>
        val Null__: Optic.Epi<Json,unit>

        val Object_: Optic.Lens<Json,JsonObject>
        val Array_: Optic.Lens<Json,Json list>
        val String_: Optic.Lens<Json,string>
        val Number_: Optic.Lens<Json,string>
        val Bool_: Optic.Lens<Json,bool>
        val Null_: Optic.Lens<Json,unit>

        val Int16_: Optic.Lens<Json,int16>
        val Int32_: Optic.Lens<Json,int>
        val Int64_: Optic.Lens<Json,int64>
        val UInt16_: Optic.Lens<Json,uint16>
        val UInt32_: Optic.Lens<Json,uint32>
        val UInt64_: Optic.Lens<Json,uint64>
        val Single_: Optic.Lens<Json,single>
        val Float_: Optic.Lens<Json,float>
        val Decimal_: Optic.Lens<Json,decimal>
        val DateTime_: Optic.Lens<Json,System.DateTime>
        val DateTimeOffset_: Optic.Lens<Json,System.DateTimeOffset>
        val Guid_: Optic.Lens<Json,System.Guid>
        val Bytes_: Optic.Lens<Json,byte array>

        val Property_: key: string -> Optic.Lens<Json,Json>

    val init: a: 'a -> Json<'a>
    val error: e: JsonFailure -> Json<'a>
    val ofResult: result: JsonResult<'a> -> Json<'a>
    val bind: a2bJ: ('a -> Json<'b>) -> aJ: Json<'a> -> Json<'b>
    val apply: aJ: Json<'a> -> a2Jb: Json<'a -> 'b> -> Json<'b>
    val map: a2b: ('a -> 'b) -> aJ: Json<'a> -> Json<'b>
    val map2: a2b2c: ('a -> 'b -> 'c) -> aJ: Json<'a> -> bJ: Json<'b> -> Json<'c>
    val map3: a2b2c2d: ('a -> 'b -> 'c -> 'd) -> aJ: Json<'a> -> bJ: Json<'b> -> cJ: Json<'c> -> Json<'d>
    val map4: a2b2c2d2x: ('a -> 'b -> 'c -> 'd -> 'x) -> aJ: Json<'a> -> bJ: Json<'b> -> cJ: Json<'c> -> dJ: Json<'d> -> Json<'x>
    val map5: a2b2c2d2x2y: ('a -> 'b -> 'c -> 'd -> 'x -> 'y) -> aJ: Json<'a> -> bJ: Json<'b> -> cJ: Json<'c> -> dJ: Json<'d> -> xJ: Json<'x> -> Json<'y>
    val map6: a2b2c2d2x2y2z: ('a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) -> aJ: Json<'a> -> bJ: Json<'b> -> cJ: Json<'c> -> dJ: Json<'d> -> xJ: Json<'x> -> yJ: Json<'y> -> Json<'z>

    module Operators =
        val inline (>>=): aJ: Json<'a> -> a2bJ: ('a -> Json<'b>) -> Json<'b>
        val inline (=<<): a2bJ: ('a -> Json<'b>) -> aJ: Json<'a> -> Json<'b>
        val inline (<*>): a2Jb: Json<'a -> 'b> -> aJ: Json<'a> -> Json<'b>
        val inline (<!>): a2b: ('a -> 'b) -> aJ: Json<'a> -> Json<'b>
        val inline ( *>): aJ: Json<'a> -> bJ: Json<'b> -> Json<'b>
        val inline (<* ): aJ: Json<'a> -> bJ: Json<'b> -> Json<'a>
        val (>=>): a2bJ: ('a -> Json<'b>) -> b2cJ: ('b -> Json<'c>) -> ('a -> Json<'c>)
        val (<=<): b2cJ: ('b -> Json<'c>) -> a2bJ: ('a -> Json<'b>) -> ('a -> Json<'c>)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonFailure =
    val toString: failure: JsonFailure -> string
    val withTag: tag: string -> a2bR: ('a -> JsonResult<'b>) -> a: 'a -> JsonResult<'b>

[<RequireQualifiedAccess>]
module JsonResult =
    val summarize: JsonResult<'a> -> string
    val apply: aR: Result<'a,'e> -> a2Rb: Result<'a -> 'b,'e> -> Result<'b,'e>
    val applyWith: c2aR: ('c -> Result<'a,'e>) -> c: 'c -> a2Rb: Result<'a -> 'b,'e> -> Result<'b,'e>
    val deserializationError<'a> : string -> JsonResult<'a>
    val withThrowingParser: parser : (string -> 'a) -> str : string -> JsonResult<'a>

type ChironDefaults = ChironDefaults with
    static member FromJson: unit -> JsonReader<unit>
    static member FromJson: bool -> JsonReader<bool>
    static member FromJson: decimal -> JsonReader<decimal>
    static member FromJson: float -> JsonReader<float>
    static member FromJson: single -> JsonReader<single>
    static member FromJson: int16 -> JsonReader<int16>
    static member FromJson: int -> JsonReader<int>
    static member FromJson: int64 -> JsonReader<int64>
    static member FromJson: uint16 -> JsonReader<uint16>
    static member FromJson: uint32 -> JsonReader<uint32>
    static member FromJson: uint64 -> JsonReader<uint64>
    static member FromJson: string -> JsonReader<string>
    static member FromJson: System.DateTime -> JsonReader<System.DateTime>
    static member FromJson: System.DateTimeOffset -> JsonReader<System.DateTimeOffset>
    static member FromJson: System.Guid -> JsonReader<System.Guid>
    static member FromJson: byte array -> JsonReader<byte array>
    static member FromJson: Json -> JsonReader<Json>
    static member inline FromJson: 'a array -> JsonReader<'a array> when ('a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)
    static member inline FromJson: 'a list -> JsonReader<'a list> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)
    static member inline FromJson: 'a option -> JsonReader<'a option> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)
    static member inline FromJson: Set<'a> -> JsonReader<Set<'a>> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)
    static member inline FromJson: Map<string,'a> -> JsonReader<Map<string,'a>> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)
    static member inline FromJson: ('a * 'b) -> JsonReader<'a * 'b> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>) and (^b or ChironDefaults) : (static member FromJson : ^b -> JsonReader<'b>)
    static member inline FromJson: ('a * 'b * 'c) -> JsonReader<'a * 'b * 'c> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>) and (^b or ChironDefaults) : (static member FromJson : ^b -> JsonReader<'b>) and (^c or ChironDefaults) : (static member FromJson : ^c -> JsonReader<'c>)
    static member inline FromJson: ('a * 'b * 'c * 'd) -> JsonReader<'a * 'b * 'c * 'd> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>) and (^b or ChironDefaults) : (static member FromJson : ^b -> JsonReader<'b>) and (^c or ChironDefaults) : (static member FromJson : ^c -> JsonReader<'c>) and (^d or ChironDefaults) : (static member FromJson : ^d -> JsonReader<'d>)
    static member inline FromJson: ('a * 'b * 'c * 'd * 'e) -> JsonReader<'a * 'b * 'c * 'd * 'e> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>) and (^b or ChironDefaults) : (static member FromJson : ^b -> JsonReader<'b>) and (^c or ChironDefaults) : (static member FromJson : ^c -> JsonReader<'c>) and (^d or ChironDefaults) : (static member FromJson : ^d -> JsonReader<'d>) and (^e or ChironDefaults) : (static member FromJson : ^e -> JsonReader<'e>)
    static member inline FromJson: ('a * 'b * 'c * 'd * 'e * 'f) -> JsonReader<'a * 'b * 'c * 'd * 'e * 'f> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>) and (^b or ChironDefaults) : (static member FromJson : ^b -> JsonReader<'b>) and (^c or ChironDefaults) : (static member FromJson : ^c -> JsonReader<'c>) and (^d or ChironDefaults) : (static member FromJson : ^d -> JsonReader<'d>) and (^e or ChironDefaults) : (static member FromJson : ^e -> JsonReader<'e>) and (^f or ChironDefaults) : (static member FromJson : ^f -> JsonReader<'f>)
    static member inline FromJson: ('a * 'b * 'c * 'd * 'e * 'f * 'g) -> JsonReader<'a * 'b * 'c * 'd * 'e * 'f * 'g> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>) and (^b or ChironDefaults) : (static member FromJson : ^b -> JsonReader<'b>) and (^c or ChironDefaults) : (static member FromJson : ^c -> JsonReader<'c>) and (^d or ChironDefaults) : (static member FromJson : ^d -> JsonReader<'d>) and (^e or ChironDefaults) : (static member FromJson : ^e -> JsonReader<'e>) and (^f or ChironDefaults) : (static member FromJson : ^f -> JsonReader<'f>) and (^g or ChironDefaults) : (static member FromJson : ^g -> JsonReader<'g>)

    static member ToJson: unit -> Json
    static member ToJson: bool -> Json
    static member ToJson: decimal -> Json
    static member ToJson: float -> Json
    static member ToJson: single -> Json
    static member ToJson: int16 -> Json
    static member ToJson: int -> Json
    static member ToJson: int64 -> Json
    static member ToJson: uint16 -> Json
    static member ToJson: uint32 -> Json
    static member ToJson: uint64 -> Json
    static member ToJson: bigint -> Json
    static member ToJson: string -> Json
    static member ToJson: System.DateTime -> Json
    static member ToJson: System.DateTimeOffset -> Json
    static member ToJson: System.Guid -> Json
    static member ToJson: byte array -> Json
    static member ToJson: Json -> Json
    static member inline ToJson: 'a array -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json)
    static member inline ToJson: 'a list -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json)
    static member inline ToJson: 'a option -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json)
    static member inline ToJson: Set<'a> -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json)
    static member inline ToJson: Map<string,'a> -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json)
    static member inline ToJson: ('a * 'b) -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json) and (^b or ChironDefaults) : (static member ToJson: ^b -> Json)
    static member inline ToJson: ('a * 'b * 'c) -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json) and (^b or ChironDefaults) : (static member ToJson: ^b -> Json) and (^c or ChironDefaults) : (static member ToJson: ^c -> Json)
    static member inline ToJson: ('a * 'b * 'c * 'd) -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json) and (^b or ChironDefaults) : (static member ToJson: ^b -> Json) and (^c or ChironDefaults) : (static member ToJson: ^c -> Json) and (^d or ChironDefaults) : (static member ToJson: ^d -> Json)
    static member inline ToJson: ('a * 'b * 'c * 'd * 'e) -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json) and (^b or ChironDefaults) : (static member ToJson: ^b -> Json) and (^c or ChironDefaults) : (static member ToJson: ^c -> Json) and (^d or ChironDefaults) : (static member ToJson: ^d -> Json) and (^e or ChironDefaults) : (static member ToJson: ^e -> Json)
    static member inline ToJson: ('a * 'b * 'c * 'd * 'e * 'f) -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json) and (^b or ChironDefaults) : (static member ToJson: ^b -> Json) and (^c or ChironDefaults) : (static member ToJson: ^c -> Json) and (^d or ChironDefaults) : (static member ToJson: ^d -> Json) and (^e or ChironDefaults) : (static member ToJson: ^e -> Json) and (^f or ChironDefaults) : (static member ToJson: ^f -> Json)
    // static member inline ToJson: ('a * 'b * 'c * 'd * 'e * 'f * 'g) -> Json when (^a or ChironDefaults) : (static member ToJson: ^a -> Json) and (^b or ChironDefaults) : (static member ToJson: ^b -> Json) and (^c or ChironDefaults) : (static member ToJson: ^c -> Json) and (^d or ChironDefaults) : (static member ToJson: ^d -> Json) and (^e or ChironDefaults) : (static member ToJson: ^e -> Json) and (^f or ChironDefaults) : (static member ToJson: ^f -> Json) and (^g or ChironDefaults) : (static member FromJson : ^g -> Json)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonObject =
    module Optics =
        val key_: key: string -> Optic.Lens<JsonObject,Json>

    val empty: JsonObject
    val add: k: string -> v: Json -> jsonObj: JsonObject -> JsonObject
    val remove: k: string -> jsonObj: JsonObject -> JsonObject
    val tryGet: k: string -> jsonObj: JsonObject -> Json option
    val tryGetOrFail: k: string -> jsonObj: JsonObject -> JsonResult<Json>
    val writeWith: writer: JsonWriter<'a> -> key: string -> value: ^a -> jsonObj: JsonObject -> JsonObject
    val inline writeWithDefault: defaults: ^def -> key: string -> value: ^a -> jsonObj: JsonObject -> JsonObject when (^a or ^def) : (static member ToJson : ^a -> Json)
    val inline write: key: string -> value: ^a -> jsonObj: JsonObject -> JsonObject when (^a or ChironDefaults) : (static member ToJson : ^a -> Json)
    val writeOptWith: writer: JsonWriter<'a> -> key: string -> value: ^a option -> jsonObj: JsonObject -> JsonObject
    val inline writeOptWithDefault: defaults: ^def -> key: string -> value: ^a option -> jsonObj: JsonObject -> JsonObject when (^a or ^def) : (static member ToJson : ^a -> Json)
    val inline writeOpt: key: string -> value: ^a option -> jsonObj: JsonObject -> JsonObject when (^a or ChironDefaults) : (static member ToJson : ^a -> Json)
    val writeObjWith: writer: ObjectWriter<'a> -> key: string -> value: ^a -> jsonObj: JsonObject -> JsonObject
    val inline writeObjWithDefault: defaults: ^def -> key: string -> value: ^a -> jsonObj: JsonObject -> JsonObject when (^a or ^def) : (static member Encode: 'a * JsonObject -> JsonObject)
    val inline writeObj: key: string -> value: ^a -> jsonObj: JsonObject -> JsonObject when (^a or ChironDefaults) : (static member Encode: 'a * JsonObject -> JsonObject)
    val writeObjOptWith: writer: ObjectWriter<'a> -> key: string -> value: ^a option -> jsonObj: JsonObject -> JsonObject
    val inline writeObjOptWithDefault: defaults: ^def -> key: string -> value: ^a option -> jsonObj: JsonObject -> JsonObject when (^a or ^def) : (static member Encode: 'a * JsonObject -> JsonObject)
    val inline writeObjOpt: key: string -> value: ^a option -> jsonObj: JsonObject -> JsonObject when (^a or ChironDefaults) : (static member Encode: 'a * JsonObject -> JsonObject)
    val inline mixinObj: writer: ObjectWriter<'a> -> value: 'a -> jsonObj: JsonObject -> JsonObject
    val mixinObjOpt: writer: ObjectWriter<'a> -> value: 'a option -> jsonObj: JsonObject -> JsonObject
    val build: writer: ObjectWriter<'a> -> value: 'a -> Json
    val readWith: reader: JsonReader<'a> -> key: string -> jsonObj: JsonObject -> JsonResult<'a>
    val inline readWithDefault: defaults: ^def -> key: string -> jsonObj: JsonObject -> JsonResult<'a> when (^a or ^def) : (static member FromJson : ^a -> JsonReader<'a>)
    val inline read: key: string -> jsonObj: JsonObject -> JsonResult<'a> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)
    val readOptWith: reader: JsonReader<'a> -> key: string -> jsonObj: JsonObject -> JsonResult<'a option>
    val inline readOptWithDefault: defaults: ^def -> key: string -> jsonObj: JsonObject -> JsonResult<'a option> when (^a or ^def) : (static member FromJson : ^a -> JsonReader<'a>)
    val inline readOpt: key: string -> jsonObj: JsonObject -> JsonResult<'a option> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)
    val optimizeRead: JsonObject -> JsonObject
    val optimizeAppend: JsonObject -> JsonObject
    val optimizeWrite: JsonObject -> JsonObject
    val toJson: JsonObject -> Json
    val toPropertyList: JsonObject -> (string * Json) list
    val ofPropertyList: (string * Json) list -> JsonObject
    val ofPropertyListWith: toJson: JsonWriter<'a> -> (string * 'a) list -> JsonObject
    val ofMap: Map<string, Json> -> JsonObject
    val ofMapWith: toJson: JsonWriter<'a> -> Map<string, 'a> -> JsonObject

[<RequireQualifiedAccess>]
module ObjectReader =
    val init: a: 'a -> ObjectReader<'a>
    val error: e: JsonFailure -> ObjectReader<'a>
    val ofResult: result: JsonResult<'a> -> ObjectReader<'a>
    val bind: a2bR: ('a -> ObjectReader<'b>) -> aR: ObjectReader<'a> -> ObjectReader<'b>
    val apply: aR: ObjectReader<'a> -> a2Rb: ObjectReader<'a -> 'b> -> ObjectReader<'b>
    val map: a2b: ('a -> 'b) -> aR: ObjectReader<'a> -> ObjectReader<'b>
    val map2: a2b2c: ('a -> 'b -> 'c) -> aR: ObjectReader<'a> -> bR: ObjectReader<'b> -> ObjectReader<'c>
    val map3: a2b2c2d: ('a -> 'b -> 'c -> 'd) -> aR: ObjectReader<'a> -> bR: ObjectReader<'b> -> cR: ObjectReader<'c> -> ObjectReader<'d>
    val map4: a2b2c2d2x: ('a -> 'b -> 'c -> 'd -> 'x) -> aR: ObjectReader<'a> -> bR: ObjectReader<'b> -> cR: ObjectReader<'c> -> dR: ObjectReader<'d> -> ObjectReader<'x>
    val map5: a2b2c2d2x2y: ('a -> 'b -> 'c -> 'd -> 'x -> 'y) -> aR: ObjectReader<'a> -> bR: ObjectReader<'b> -> cR: ObjectReader<'c> -> dR: ObjectReader<'d> -> xR: ObjectReader<'x> -> ObjectReader<'y>
    val map6: a2b2c2d2x2y2z: ('a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) -> aR: ObjectReader<'a> -> bR: ObjectReader<'b> -> cR: ObjectReader<'c> -> dR: ObjectReader<'d> -> xR: ObjectReader<'x> -> yR: ObjectReader<'y> -> ObjectReader<'z>
    val toJsonReader: aR: ObjectReader<'a> -> JsonReader<'a>

    module Operators =
        val inline (>>=): aR: ObjectReader<'a> -> a2bR: ('a -> ObjectReader<'b>) -> ObjectReader<'b>
        val inline (=<<): a2bR: ('a -> ObjectReader<'b>) -> aR: ObjectReader<'a> -> ObjectReader<'b>
        val inline (<*>): a2Rb: ObjectReader<'a -> 'b> -> aR: ObjectReader<'a> -> ObjectReader<'b>
        val inline (<!>): a2b: ('a -> 'b) -> aR: ObjectReader<'a> -> ObjectReader<'b>
        val inline ( *>): aR: ObjectReader<'a> -> bR: ObjectReader<'b> -> ObjectReader<'b>
        val inline (<* ): aR: ObjectReader<'a> -> bR: ObjectReader<'b> -> ObjectReader<'a>
        val (>=>): a2bR: ('a -> ObjectReader<'b>) -> b2cR: ('b -> ObjectReader<'c>) -> ('a -> ObjectReader<'c>)
        val (<=<): b2cR: ('b -> ObjectReader<'c>) -> a2bR: ('a -> ObjectReader<'b>) -> ('a -> ObjectReader<'c>)

[<RequireQualifiedAccess>]
module JsonReader =
    val init: a: 'a -> JsonReader<'a>
    val error: e: JsonFailure -> JsonReader<'a>
    val ofResult: result: JsonResult<'a> -> JsonReader<'a>
    val bind: a2bR: ('a -> JsonReader<'b>) -> aR: JsonReader<'a> -> JsonReader<'b>
    val apply: aR: JsonReader<'a> -> a2Rb: JsonReader<'a -> 'b> -> JsonReader<'b>
    val map: a2b: ('a -> 'b) -> aR: JsonReader<'a> -> JsonReader<'b>
    val map2: a2b2c: ('a -> 'b -> 'c) -> aR: JsonReader<'a> -> bR: JsonReader<'b> -> JsonReader<'c>
    val map3: a2b2c2d: ('a -> 'b -> 'c -> 'd) -> aR: JsonReader<'a> -> bR: JsonReader<'b> -> cR: JsonReader<'c> -> JsonReader<'d>
    val map4: a2b2c2d2x: ('a -> 'b -> 'c -> 'd -> 'x) -> aR: JsonReader<'a> -> bR: JsonReader<'b> -> cR: JsonReader<'c> -> dR: JsonReader<'d> -> JsonReader<'x>
    val map5: a2b2c2d2x2y: ('a -> 'b -> 'c -> 'd -> 'x -> 'y) -> aR: JsonReader<'a> -> bR: JsonReader<'b> -> cR: JsonReader<'c> -> dR: JsonReader<'d> -> xR: JsonReader<'x> -> JsonReader<'y>
    val map6: a2b2c2d2x2y2z: ('a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) -> aR: JsonReader<'a> -> bR: JsonReader<'b> -> cR: JsonReader<'c> -> dR: JsonReader<'d> -> xR: JsonReader<'x> -> yR: JsonReader<'y> -> JsonReader<'z>
    val toJson: aR: JsonReader<'a> -> Json<'a>

[<AutoOpen>]
module Serialization =
    [<RequireQualifiedAccess>]
    module Json =
        val inline encode: a: 'a -> Json when ^a : (static member Encode: 'a * JsonObject -> JsonObject)

        val deserializeWith: deserializer: ('a -> JsonReader<'a>) -> JsonReader<'a>
        val inline deserializeWithDefaults: defaults: ^def -> dummy: ^a -> JsonReader<'a> when (^a or ^def) : (static member FromJson : ^a -> JsonReader<'a>)
        val inline deserialize: json: Json -> JsonResult<'a> when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)

        val inline deserializeList: init: 'a -> fold: ('b -> 'a -> 'a) -> jsons: Json list -> JsonResult<'a> when (^b or ChironDefaults) : (static member FromJson : ^b -> JsonReader<'b>)

        val inline serializeWithDefaults: defaults: ^def -> JsonWriter<'a> when (^a or ^def) : (static member ToJson : 'a -> Json)
        val inline serialize: a: 'a -> Json when (^a or ChironDefaults) : (static member ToJson : 'a -> Json)

[<AutoOpen>]
module Builders =
    type JsonReaderBuilder =
        new: unit -> JsonReaderBuilder
        member Return: a: 'a -> ObjectReader<'a>
        member ReturnFrom: aR: ObjectReader<'a> -> ObjectReader<'a>
        member Bind: aR: ObjectReader<'a> * a2bR: ('a -> ObjectReader<'b>) -> ObjectReader<'b>
        // member Bind: aR: JsonReader<'a> * a2bR: ('a -> ObjectReader<'b>) -> ObjectReader<'b>
        // member Bind: aR: ObjectReader<'a> * a2bR: ('a -> JsonReader<'b>) -> ObjectReader<'b>
        member Zero: unit -> ObjectReader<unit>
        member Combine: aR: ObjectReader<'a> * bR: ObjectReader<'b> -> ObjectReader<'b>
        member Delay: u2bR: (unit -> ObjectReader<'b>) -> ObjectReader<'b>
        member Run: aM: ObjectReader<'a> -> JsonReader<'a>

    // type JsonWriterBuilder =
    //     new: unit -> JsonWriterBuilder
    //     member Zero: unit -> JsonObject
    //     member Bind: j2j: (JsonObject -> JsonObject) * jObj: JsonObject -> JsonObject
    //     member Run: jObj: JsonObject -> Json

    type JsonBuilder =
        new: unit -> JsonBuilder
        member Return: a: 'a -> Json<'a>
        member ReturnFrom: aJ: Json<'a> -> Json<'a>
        member Bind: aJ: Json<'a> * a2bJ: ('a -> Json<'b>) -> Json<'b>
        member Zero: unit -> Json<unit>
        member Combine: aJ: Json<'a> * bJ: Json<'b> -> Json<'b>
        member Delay: u2bJ: (unit -> Json<'b>) -> Json<'b>

    val jsonReader: JsonReaderBuilder
    // val jsonWriter: JsonWriterBuilder
    val json: JsonBuilder

[<AutoOpen>]
module Patterns =
    val (|Object|Array|String|Number|Bool|Null|): Json -> Choice<JsonObject,Json list,string,string,bool,unit>
    val (|PropertyWith|_|): reader: JsonReader<'a> -> k: string -> Json -> 'a option
    val inline (|Property|_|): k: string -> Json -> 'a option when (^a or ChironDefaults) : (static member FromJson : ^a -> JsonReader<'a>)
