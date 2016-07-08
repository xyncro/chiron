module JsonSchema

open Chiron

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
      pattern: System.Text.RegularExpressions.Regex option
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
        let! patternStr = Json.readOrDefault "pattern" None
        let! pattern = ifProvided tryParseJsonToRegex patternStr
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
and BoolOrSchema =
    | Allowed of bool
    | ElementSchema of JsonSchema
    static member FromJson (_:BoolOrSchema) : Json<BoolOrSchema> = fun json ->
        match json with
        | Json.Bool b -> Json.init (Allowed b) json
        | Json.Object _ -> (Json.readMixin |> Json.map ElementSchema) json
        | _ -> Json.error "Expected either a boolean or a schema object" json
and SchemaOrSchemaArray =
    | SingleSchema of JsonSchema
    | MultipleSchemas of JsonSchema list
    static member FromJson (_:SchemaOrSchemaArray) : Json<SchemaOrSchemaArray> = fun json ->
        match json with
        | Json.Object _ -> (Json.readMixin |> Json.map SingleSchema) json
        | Json.Array _ -> (FromJsonDefaults.FromJson Unchecked.defaultof<JsonSchema list> |> Json.map MultipleSchemas) json
        | _ -> Json.error "Expected either a schema object or a list of schema objects" json
and SchemaOrPropertyNames =
    | DependentSchema of JsonSchema
    | DependentProperties of PropertyName list
    static member FromJson (_:SchemaOrPropertyNames) : Json<SchemaOrPropertyNames> = fun json ->
        match json with
        | Json.Object _ -> (Json.readMixin |> Json.map DependentSchema) json
        | Json.Array _ ->  (FromJsonDefaults.FromJson Unchecked.defaultof<PropertyName list> |> Json.map DependentProperties) json
        | _ -> Json.error "Expected either a schema object or an array of property names" json
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
and JsonTypeOrTypes =
    | SingleType of JsonType
    | MultipleTypes of JsonType list
    static member FromJson (_:JsonTypeOrTypes) : Json<JsonTypeOrTypes> = fun json ->
        match json with
        | Json.String _ -> (Json.readMixin |> Json.map SingleType) json
        | Json.Array _ -> (FromJsonDefaults.FromJson Unchecked.defaultof<JsonType list> |> Json.map MultipleTypes) json
        | _ -> Json.error "Expected either a JSON type or a list of JSON types" json
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

let jsonSchema =
    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    let resource = "json-schema.json"
    use stream = assembly.GetManifestResourceStream resource
    use reader = new System.IO.StreamReader(stream)
    reader.ReadToEnd()

let jsonSchemaJson =
    Json.parse jsonSchema

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
        Json.tryParse jsonSchema |> ignore
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