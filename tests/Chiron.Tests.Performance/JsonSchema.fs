module JsonSchema

open Chiron

let inline flip f x y = f y x

let tryParseJsonToUri str =
    match System.Uri.TryCreate (str, System.UriKind.RelativeOrAbsolute) with
    | true, uri -> Json.init uri
    | false, _ -> Json.error ("Unable to parse string as a Uri: " + str)

let tryParseJsonToRegex str =
    try Json.init <| System.Text.RegularExpressions.Regex (str, System.Text.RegularExpressions.RegexOptions.ECMAScript)
    with
    | :? System.ArgumentException as e ->
        Json.error ("Error converting pattern to regular expression: " + e.Message)

let requireDependant opt dep optName depName =
    match opt, dep with
    | Some _, None -> Json.error (sprintf "Missing dependant property: '%s' depends on '%s'" optName depName)
    | _ -> Json.init ()

let ifProvided f = function
    | Some x -> f x |> Json.map Some
    | None -> Json.init None

module Json =
    let inline readMixin json =
        (^a : (static member FromJson: ^a -> Json<'a>) Unchecked.defaultof<_>) json

    let inline writeMixin x json =
        (^a : (static member ToJson: ^a -> Json<unit>) x) json

type BoundaryType = Inclusive | Exclusive

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BoundaryType =
    let ofBool = function
    | true -> Exclusive
    | false -> Inclusive

type NumericBoundary =
    { limit : decimal
      boundaryType : BoundaryType }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NumericBoundary =
    let tryMake limit : Json -> JsonResult<NumericBoundary> =
        Unchecked.defaultof<_>

type JsonRegex =
    { originalString : string
      regex : System.Text.RegularExpressions.Regex }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonRegex =
    let tryMake (str : string) : Choice<JsonRegex,string> =
        try
            let regex = System.Text.RegularExpressions.Regex (str, System.Text.RegularExpressions.RegexOptions.ECMAScript)
            { originalString = str; regex = regex }
            |> Choice1Of2
        with
        | :? System.ArgumentException as e ->
            Choice2Of2 <| "Error converting pattern to regular expression: " + e.Message            
    let decode : Json -> JsonResult<JsonRegex> = function
        | Json.String str ->
            tryMake str |> function Choice1Of2 v -> JsonResult.Value v | Choice2Of2 e -> JsonResult.Error e
        | json -> JsonResult.Error ("Expected a JSON string, but found: " + Json.format json)
    let encode (jregex : JsonRegex) : Json =
        Json.String jregex.originalString

type PropertyName = string
type JsonSchemaDefinition =
    { id : System.Uri option
      ``$schema`` : System.Uri option
      title : string option
      description: string option
      ``default`` : Json option
      multipleOf: decimal option
      maximum: decimal option
      exclusiveMaximum: bool
      minimum: decimal option
      exclusiveMinimum: bool
      maxLength: uint64 option
      minLength: uint64
      pattern: JsonRegex option
      additionalItems: BoolOrSchema
      items : SchemaOrSchemaArray
      maxItems : uint64 option
      minItems : uint64
      uniqueItems : bool
      maxProperties: uint64 option
      minProperties: uint64
      required: PropertyName list option
      additionalProperties: BoolOrSchema
      definitions: Map<string, JsonSchema>
      properties: Map<PropertyName, JsonSchema>
      patternProperties: Map<string, JsonSchema>
      dependencies: Map<PropertyName, SchemaOrPropertyNames> option
      enum : Json list option
      ``type`` : JsonTypeOrTypes option
      allOf: JsonSchema list option
      anyOf: JsonSchema list option
      oneOf: JsonSchema list option
      not : JsonSchema option }
    static member FromJson (_:JsonSchemaDefinition) : Json<JsonSchemaDefinition> = json {
        let! schemaIdStr = Json.readOrDefault "id" None
        let! schemaId = ifProvided tryParseJsonToUri schemaIdStr
        let! ``$schemaStr`` = Json.readOrDefault "schema" None
        let! ``$schema`` = ifProvided tryParseJsonToUri ``$schemaStr``
        let! title = Json.readOrDefault "title" None
        let! description = Json.readOrDefault "description" None
        let! ``default`` = Json.readOrDefault "default" None
        let! multipleOf = Json.readOrDefault "multipleOf" None
        let! maximum = Json.readOrDefault "maximum" None
        let! exclusiveMaximumOpt = Json.readOrDefault "exclusiveMaximum" None
        do! requireDependant exclusiveMaximumOpt maximum "exclusiveMaximum" "maximum" 
        let exclusiveMaximum = defaultArg exclusiveMaximumOpt false
        let! minimum = Json.readOrDefault "minimum" None
        let! exclusiveMinimumOpt = Json.readOrDefault "exclusiveMinimum" None
        do! requireDependant exclusiveMinimumOpt minimum "exclusiveMinimum" "minimum"
        let exclusiveMinimum = defaultArg exclusiveMinimumOpt false
        let! maxLength = Json.readOrDefault "maxLength" None
        let! minLength = Json.readOrDefault "minLength" 0UL
        let! (pattern : JsonRegex option) = Json.readWithOrDefault (FromJsonDefaults.FromJson Unchecked.defaultof<Json option> |> flip Json.bind (fun (json : Json option) (j : Json) -> match json with None -> JsonResult.Value None, j | Some js -> match JsonRegex.decode js with JsonResult.Value v -> JsonResult.Value (Some v), j | JsonResult.Error err -> JsonResult.Error err, j) >> fst) "pattern" None
        let! additionalItems = Json.readOrDefault "additionalItems" (ElementSchema EmptySchema)
        let! items = Json.readOrDefault "items" (SingleSchema EmptySchema)
        let! maxItems = Json.readOrDefault "maxItems" None
        let! minItems = Json.readOrDefault "minItems" 0UL
        let! uniqueItems = Json.readOrDefault "uniqueItems" false
        let! maxProperties = Json.readOrDefault "maxProperties" None
        let! minProperties = Json.readOrDefault "minProperties" 0UL
        let! required = Json.readOrDefault "required" None
        let! additionalProperties = Json.readOrDefault "additionalProperties" (ElementSchema EmptySchema)
        let! definitions = Json.readOrDefault "definitions" Map.empty
        let! properties = Json.readOrDefault "properties" Map.empty
        let! patternProperties = Json.readOrDefault "patternProperties" Map.empty
        let! dependencies = Json.readOrDefault "dependencies" None
        let! enum = Json.readOrDefault "enum" None
        let! ``type`` = Json.readOrDefault "type" None
        let! allOf = Json.readOrDefault "allOf" None
        let! anyOf = Json.readOrDefault "anyOf" None
        let! oneOf = Json.readOrDefault "oneOf" None
        let! not = Json.readOrDefault "not" None
        return
            { id = schemaId
              ``$schema`` = ``$schema``
              title = title
              description = description
              ``default`` = ``default``
              multipleOf = multipleOf
              maximum = maximum
              exclusiveMaximum = exclusiveMaximum
              minimum = minimum
              exclusiveMinimum = exclusiveMinimum
              maxLength = maxLength
              minLength = minLength
              pattern = pattern
              additionalItems = additionalItems
              items = items
              maxItems = maxItems
              minItems = minItems
              uniqueItems = uniqueItems
              maxProperties = maxProperties
              minProperties = minProperties
              required = required
              additionalProperties = additionalProperties
              definitions = definitions
              properties = properties
              patternProperties = patternProperties
              dependencies = dependencies
              enum = enum
              ``type`` = ``type``
              allOf = allOf
              anyOf = anyOf
              oneOf = oneOf
              not = not }
    }
    static member ToJson (x:JsonSchemaDefinition) : Json<unit> = json {
        do! Json.writeUnlessDefault "id" None (Option.map string x.id)
        do! Json.writeUnlessDefault "$schema" None (Option.map string x.``$schema``)
        do! Json.writeUnlessDefault "title" None x.title
        do! Json.writeUnlessDefault "description" None x.description
        do! Json.writeUnlessDefault "default" None x.``default``
        do! Json.writeUnlessDefault "multipleOf" None x.multipleOf
        do! Json.writeUnlessDefault "maximum" None x.maximum
        do! Json.writeUnlessDefault "exculsiveMaximum" false x.exclusiveMaximum
        do! Json.writeUnlessDefault "minimum" None x.minimum
        do! Json.writeUnlessDefault "exclusiveMinimum" false x.exclusiveMinimum
        do! Json.writeUnlessDefault "maxLength" None x.maxLength
        do! Json.writeUnlessDefault "minLength" 0UL x.minLength
        do! Json.writeWithUnlessDefault (function Some v -> JsonRegex.encode v | None -> Json.Null ()) "pattern" None x.pattern
        do! Json.writeUnlessDefault "additionalItems" (ElementSchema EmptySchema) x.additionalItems
        do! Json.writeUnlessDefault "items" (SingleSchema EmptySchema) x.items
        do! Json.writeUnlessDefault "maxItems" None x.maxItems
        do! Json.writeUnlessDefault "minItems" 0UL x.minItems
        do! Json.writeUnlessDefault "uniqueItems" false x.uniqueItems
        do! Json.writeUnlessDefault "maxProperties" None x.maxProperties
        do! Json.writeUnlessDefault "minProperties" 0UL x.minProperties
        do! Json.writeUnlessDefault "required" None x.required
        do! Json.writeUnlessDefault "additionalProperties" (ElementSchema EmptySchema) x.additionalProperties
        do! Json.writeUnlessDefault "definitions" Map.empty x.definitions
        do! Json.writeUnlessDefault "properties" Map.empty x.properties
        do! Json.writeUnlessDefault "patternProperties" Map.empty x.patternProperties
        do! Json.writeUnlessDefault "dependencies" None x.dependencies
        do! Json.writeUnlessDefault "enum" None x.enum
        do! Json.writeUnlessDefault "type" None x.``type``
        do! Json.writeUnlessDefault "allOf" None x.allOf
        do! Json.writeUnlessDefault "anyOf" None x.anyOf
        do! Json.writeUnlessDefault "oneOf" None x.oneOf
        do! Json.writeUnlessDefault "not" None x.not
    }
and BoolOrSchema =
    | Allowed of bool
    | ElementSchema of JsonSchema
    static member FromJson (_:BoolOrSchema) : Json<BoolOrSchema> = fun json ->
        match json with
        | Json.Bool b -> Json.init (Allowed b) json
        | Json.Object _ -> (Json.readMixin |> Json.map ElementSchema) json
        | _ -> Json.error "Expected either a boolean or a schema object" json
    static member ToJson (x:BoolOrSchema) : Json<unit> =
        match x with
        | Allowed b -> ToJsonDefaults.ToJson b
        | ElementSchema s -> Json.writeMixin s
and SchemaOrSchemaArray =
    | SingleSchema of JsonSchema
    | MultipleSchemas of JsonSchema list
    static member FromJson (_:SchemaOrSchemaArray) : Json<SchemaOrSchemaArray> = fun json ->
        match json with
        | Json.Object _ -> (Json.readMixin |> Json.map SingleSchema) json
        | Json.Array _ -> (FromJsonDefaults.FromJson Unchecked.defaultof<JsonSchema list> |> Json.map MultipleSchemas) json
        | _ -> Json.error "Expected either a schema object or a list of schema objects" json
    static member ToJson (x:SchemaOrSchemaArray) : Json<unit> =
        match x with
        | SingleSchema s -> Json.writeMixin s
        | MultipleSchemas ms -> ToJsonDefaults.ToJson ms
and SchemaOrPropertyNames =
    | DependentSchema of JsonSchema
    | DependentProperties of PropertyName list
    static member FromJson (_:SchemaOrPropertyNames) : Json<SchemaOrPropertyNames> = fun json ->
        match json with
        | Json.Object _ -> (Json.readMixin |> Json.map DependentSchema) json
        | Json.Array _ ->  (FromJsonDefaults.FromJson Unchecked.defaultof<PropertyName list> |> Json.map DependentProperties) json
        | _ -> Json.error "Expected either a schema object or an array of property names" json
    static member ToJson (x:SchemaOrPropertyNames) : Json<unit> =
        match x with
        | DependentSchema s -> Json.writeMixin s
        | DependentProperties ps -> ToJsonDefaults.ToJson ps
and JsonType =
    | JsonObject
    | JsonArray
    | JsonString
    | JsonNumber
    | JsonInteger
    | JsonBoolean
    | JsonNull
    static member FromJson (_:JsonType) : Json<JsonType> = json {
        let! v = Json.Optic.get Json.String_
        return!
            match v with
            | "object" -> Json.init JsonObject
            | "array" -> Json.init JsonArray
            | "number" -> Json.init JsonNumber
            | "integer" -> Json.init JsonInteger
            | "boolean" -> Json.init JsonBoolean
            | "string" -> Json.init JsonString
            | "null" -> Json.init JsonNull
            | _ -> Json.error ("Invalid JSON type: " + v)
    }
    static member ToJson (x:JsonType) : Json<unit> =
        match x with
        | JsonObject -> "object"
        | JsonArray -> "array"
        | JsonNumber -> "number"
        | JsonInteger -> "integer"
        | JsonString -> "string"
        | JsonBoolean -> "boolean"
        | JsonNull -> "null"
        |> Json.Optic.set Json.String_
and JsonTypeOrTypes =
    | SingleType of JsonType
    | MultipleTypes of JsonType list
    static member FromJson (_:JsonTypeOrTypes) : Json<JsonTypeOrTypes> = fun json ->
        match json with
        | Json.String _ -> (Json.readMixin |> Json.map SingleType) json
        | Json.Array _ -> (FromJsonDefaults.FromJson Unchecked.defaultof<JsonType list> |> Json.map MultipleTypes) json
        | _ -> Json.error "Expected either a JSON type or a list of JSON types" json
    static member ToJson (x:JsonTypeOrTypes) : Json<unit> =
        match x with
        | SingleType jt -> Json.writeMixin jt
        | MultipleTypes mt -> ToJsonDefaults.ToJson mt
and JsonSchema =
    | EmptySchema
    | SchemaReference of System.Uri
    | Schema of JsonSchemaDefinition
    static member FromJson (_:JsonSchema) : Json<JsonSchema> = json {
        let! properties = Json.Optic.get Json.Object_
        match Map.foldBack (fun _ _ -> (+) 1UL) properties 0UL with
        | 0UL -> return EmptySchema
        | 1UL when Map.containsKey "$ref" properties ->
            let! refStr = Json.read "$ref"
            let! ref = tryParseJsonToUri refStr
            return SchemaReference ref
        | _ ->
            let! s = Json.readMixin
            return Schema s
    }
    static member ToJson (x:JsonSchema) : Json<unit> =
        match x with
        | EmptySchema -> Json.init ()
        | SchemaReference uri -> Json.write "$ref" (string uri)
        | Schema sd -> Json.writeMixin sd

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonSchemaDefinition =
    let empty : JsonSchemaDefinition =
        { id = None; ``$schema`` = None; title = None; description = None
          ``default`` = None; multipleOf = None; maximum = None
          exclusiveMaximum = false; minimum = None; exclusiveMinimum = false
          maxLength = None; minLength = 0UL; pattern = None
          additionalItems = ElementSchema EmptySchema
          items = SingleSchema EmptySchema
          maxItems = None; minItems = 0UL; uniqueItems = false
          maxProperties = None; minProperties = 0UL; required = None
          additionalProperties = ElementSchema EmptySchema
          definitions = Map.empty
          properties = Map.empty
          patternProperties = Map.empty
          dependencies = None; enum = None; ``type`` = None
          allOf = None; anyOf = None; oneOf = None; not = None }

let jsonSchemaStr : string =
    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    let resource = "json-schema.json"
    use stream = assembly.GetManifestResourceStream resource
    use reader = new System.IO.StreamReader(stream)
    reader.ReadToEnd()

let jsonSchemaBytes : byte array =
    System.Text.Encoding.UTF8.GetBytes jsonSchemaStr

let jsonSchemaJson : Json =
    Json.parse jsonSchemaStr

let jsonSchema : JsonSchema =
    Json.deserialize jsonSchemaJson

open NBench
open NBench.Util

type Parse() =
    let mutable counter : Counter = Unchecked.defaultof<_>

    [<PerfSetup>]
    member __.Setup (ctx : BenchmarkContext) : unit =
        counter <- getCounter ctx "TestCounter"

    [<PerfBenchmark(
        Description = "Parse",
        NumberOfIterations = 10,
        RunMode = RunMode.Throughput,
        RunTimeMilliseconds = 1000,
        TestMode = TestMode.Measurement)>]
    [<CounterThroughputAssertion("TestCounter", MustBe.GreaterThan, 1000.)>]
    member __.Benchmark (ctx : BenchmarkContext) : unit =
        Json.tryParse jsonSchemaStr |> ignore
        incr counter

type Deserialize() =
    let mutable counter : Counter = Unchecked.defaultof<_>

    [<PerfSetup>]
    member __.Setup (ctx : BenchmarkContext) : unit =
        counter <- getCounter ctx "TestCounter"

    [<PerfBenchmark(
        Description = "Deserialize",
        NumberOfIterations = 10,
        RunMode = RunMode.Throughput,
        RunTimeMilliseconds = 1000,
        TestMode = TestMode.Measurement)>]
    [<CounterThroughputAssertion("TestCounter", MustBe.GreaterThan, 1000.)>]
    member __.Benchmark (ctx : BenchmarkContext) : unit =
        (Json.tryDeserialize jsonSchemaJson : Choice<JsonSchema,_>) |> ignore
        incr counter

type Serialize() =
    let mutable counter : Counter = Unchecked.defaultof<_>

    [<PerfSetup>]
    member __.Setup (ctx : BenchmarkContext) : unit =
        counter <- getCounter ctx "TestCounter"

    [<PerfBenchmark(
        Description = "Serialize",
        NumberOfIterations = 10,
        RunMode = RunMode.Throughput,
        RunTimeMilliseconds = 1000,
        TestMode = TestMode.Measurement)>]
    [<CounterThroughputAssertion("TestCounter", MustBe.GreaterThan, 1000.)>]
    member __.Benchmark (ctx : BenchmarkContext) : unit =
        Json.serialize jsonSchema |> ignore
        incr counter

type Format() =
    let mutable counter : Counter = Unchecked.defaultof<_>

    [<PerfSetup>]
    member __.Setup (ctx : BenchmarkContext) : unit =
        counter <- getCounter ctx "TestCounter"

    [<PerfBenchmark(
        Description = "Format",
        NumberOfIterations = 10,
        RunMode = RunMode.Throughput,
        RunTimeMilliseconds = 1000,
        TestMode = TestMode.Measurement)>]
    [<CounterThroughputAssertion("TestCounter", MustBe.GreaterThan, 1000.)>]
    member __.Benchmark (ctx : BenchmarkContext) : unit =
        Json.format jsonSchemaJson |> ignore
        incr counter
