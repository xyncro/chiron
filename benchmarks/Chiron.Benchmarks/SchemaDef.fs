module JsonSchema

#nowarn "40"

open Chiron

module E = Json.Encode
module D = Json.Decode
module JO = JsonObject

type BoundaryType = Inclusive | Exclusive

module BoundaryType =
    let isExclusive = function
        | Exclusive -> true
        | _ -> false

    let ofExclusiveBool = function
        | true -> Exclusive
        | _ -> Inclusive

type NumericBoundary =
    { limit : decimal
      boundaryType : BoundaryType }

type JsonSchemaType =
    | Object
    | Array
    | String
    | Number
    | Integer
    | Boolean
    | Null

type JsonRegex = JsonRegex of System.Text.RegularExpressions.Regex

type PropertyName = string

type JsonSchemaDefinition =
    { id : System.Uri option
      ``$schema`` : System.Uri option
      title : string option
      description: string option
      ``default`` : Json option
      multipleOf: decimal option
      maximum: NumericBoundary option
      minimum: NumericBoundary option
      maxLength: uint64 option
      minLength: uint64
      pattern: JsonRegex option
      additionalItems: AdditionalElements
      items : ArrayItemsSchema option
      maxItems : uint64 option
      minItems : uint64
      uniqueItems : bool
      maxProperties: uint64 option
      minProperties: uint64
      required: PropertyName list
      additionalProperties: AdditionalElements
      definitions: Map<string, JsonSchema>
      properties: Map<PropertyName, JsonSchema>
      patternProperties: (JsonRegex * JsonSchema) list
      dependencies: Map<PropertyName, Dependencies>
      enum : Json list
      ``type`` : JsonSchemaType list
      allOf: JsonSchema list
      anyOf: JsonSchema list
      oneOf: JsonSchema list
      not : JsonSchema option }
and JsonSchema =
    | EmptySchema
    | SchemaReference of System.Uri
    | Schema of JsonSchemaDefinition
and ArrayItemsSchema =
    | ItemsSchema of JsonSchema
    | TupleSchema of JsonSchema list
and AdditionalElements =
    | Allowed of bool
    | ElementSchema of JsonSchema
and Dependencies =
    | DependentSchema of JsonSchema
    | DependentProperties of PropertyName list

module AdditionalElements =
    let empty = Allowed true

module JsonRegex =
    let get (JsonRegex r) = r
    let pattern (JsonRegex r) = r.ToString()

module JsonSchemaDefinition =
    let empty : JsonSchemaDefinition =
        { id = None; ``$schema`` = None; title = None; description = None
          ``default`` = None; multipleOf = None; maximum = None; minimum = None
          maxLength = None; minLength = 0UL; pattern = None
          additionalItems = AdditionalElements.empty
          items = None; maxItems = None; minItems = 0UL; uniqueItems = false
          maxProperties = None; minProperties = 0UL; required = []
          additionalProperties = AdditionalElements.empty
          definitions = Map.empty
          properties = Map.empty
          patternProperties = []
          dependencies = Map.empty; enum = []; ``type`` = []
          allOf = []; anyOf = []; oneOf = []; not = None }

module JsonObject =
    let dependent (primaryKey: string) (primaryReader: JsonReader<'a>) (dependentKey: string) (dependentReader: JsonReader<'b>) : ObjectReader<('a * 'b option) option> =
        let primaryObjectReader = JO.readOptionalWith primaryReader primaryKey
        let dependentObjectReader = JO.readOptionalWith dependentReader dependentKey
        fun jObj ->
            let aOR = primaryObjectReader jObj
            let bOR = dependentObjectReader jObj
            match aOR, bOR with
            | JsonResult.Ok (Some a), JsonResult.Ok bO ->
                JsonResult.Ok (Some (a, bO))
            | JsonResult.Ok None, JsonResult.Ok (Some _) ->
                JsonResult.Error [DeserializationError (typeof<'b>, sprintf "Property '%s' requires property '%s' to be specified" dependentKey primaryKey)]
            | JsonResult.Ok None, JsonResult.Ok None ->
                JsonResult.Ok None
            | JsonResult.Error err1, JsonResult.Error err2 ->
                JsonResult.Error (err1 @ err2)
            | JsonResult.Error err, JsonResult.Ok _
            | JsonResult.Ok _, JsonResult.Error err ->
                JsonResult.Error err

module JO = JsonObject

module Json =
    module Encode =
        let uri (u: System.Uri) = E.string (u.ToString())
        let boundaryType (bt: BoundaryType) =
            E.bool (BoundaryType.isExclusive bt)
        let regex (r: JsonRegex) =
            E.string (JsonRegex.pattern r)
        let jsonSchemaType = function
            | JsonSchemaType.Object -> E.string "object"
            | JsonSchemaType.Array -> E.string "array"
            | JsonSchemaType.String -> E.string "string"
            | JsonSchemaType.Number -> E.string "number"
            | JsonSchemaType.Integer -> E.string "integer"
            | JsonSchemaType.Boolean -> E.string "boolean"
            | JsonSchemaType.Null -> E.string "null"
        let jsonSchemaTypeOrTypes = function
            | [jmt] -> jsonSchemaType jmt
            | jmts -> E.listWith jsonSchemaType jmts
        let schemaRef =
            let objectWriter (u: System.Uri) jObj =
                jObj |> JO.writeWith uri "$ref" u
            JsonObject.buildWith objectWriter
        let inline writeUnlessDefaultWith encode def k v jObj =
            if v = def then jObj
            else JO.writeWith encode k v jObj
        let rec jsonSchema =
            let jsonSchemaDefinitionDelayed = fun x -> jsonSchemaDefinitionDelayed () x
            function
            | EmptySchema -> E.jsonObject JsonObject.empty
            | SchemaReference sr -> schemaRef sr
            | Schema sd -> jsonSchemaDefinitionDelayed sd
        and regexSchemaList =
            let folder =
                (do ())
                fun (r, s) jObj ->
                    JsonObject.add (JsonRegex.pattern r) (jsonSchema s) jObj
            let objectWriter rsl jObj =
                List.foldBack folder rsl jObj
            JsonObject.buildWith objectWriter
        and additionalElements = function
            | Allowed b -> E.bool b
            | ElementSchema s -> jsonSchema s
        and arrayItemsSchema = function
            | ItemsSchema s -> jsonSchema s
            | TupleSchema ms -> E.listWith jsonSchema ms
        and dependencies = function
            | DependentSchema s -> jsonSchema s
            | DependentProperties dp -> E.listWith E.string dp
        and jsonSchemaDefinitionDelayed () =
            let getLimit = (do ()); fun {limit=l} -> l
            let getBoundType = (do ()); fun {boundaryType=bt} -> bt
            let stringList = E.listWith E.string
            let jsonSchemaMap = E.mapWith jsonSchema
            let jsonSchemaList = E.listWith jsonSchema
            let dependenciesMap = E.mapWith dependencies
            let objectWriter =
                fun (jsd: JsonSchemaDefinition) jObj ->
                    jObj
                    |> JO.writeOptionalWith uri "id" jsd.id
                    |> JO.writeOptionalWith uri "$schema" jsd.``$schema``
                    |> JO.writeOptionalWith E.string "title" jsd.title
                    |> JO.writeOptionalWith E.string "description" jsd.description
                    |> JO.writeOptionalWith E.json "default" jsd.``default``
                    |> JO.writeOptionalWith E.decimal "multipleOf" jsd.multipleOf
                    |> JO.writeOptionalWith E.decimal "maximum" (jsd.maximum |> Option.map getLimit)
                    |> writeUnlessDefaultWith boundaryType Inclusive "exclusiveMaximum" (jsd.minimum |> Option.map getBoundType |> Option.defaultValue Inclusive)
                    |> JO.writeOptionalWith E.decimal "minimum" (jsd.minimum |> Option.map getLimit)
                    |> writeUnlessDefaultWith boundaryType Inclusive "exclusiveMinimum" (jsd.minimum |> Option.map getBoundType |> Option.defaultValue Inclusive)
                    |> JO.writeOptionalWith E.uint64 "maxLength" jsd.maxLength
                    |> writeUnlessDefaultWith E.uint64 0UL "minLength" jsd.minLength
                    |> JO.writeOptionalWith regex "pattern" jsd.pattern
                    |> writeUnlessDefaultWith additionalElements AdditionalElements.empty "additionalItems" jsd.additionalItems
                    |> JO.writeOptionalWith arrayItemsSchema "items" jsd.items
                    |> JO.writeOptionalWith E.uint64 "maxItems" jsd.maxItems
                    |> writeUnlessDefaultWith E.uint64 0UL "minItems" jsd.minItems
                    |> writeUnlessDefaultWith E.bool false "uniqueItems" jsd.uniqueItems
                    |> JO.writeOptionalWith E.uint64 "maxProperties" jsd.maxProperties
                    |> writeUnlessDefaultWith E.uint64 0UL "minProperties" jsd.minProperties
                    |> writeUnlessDefaultWith stringList [] "required" jsd.required
                    |> writeUnlessDefaultWith additionalElements AdditionalElements.empty "additionalProperties" jsd.additionalProperties
                    |> writeUnlessDefaultWith jsonSchemaMap Map.empty "definitions" jsd.definitions
                    |> writeUnlessDefaultWith jsonSchemaMap Map.empty "properties" jsd.properties
                    |> writeUnlessDefaultWith regexSchemaList [] "patternProperties" jsd.patternProperties
                    |> writeUnlessDefaultWith dependenciesMap Map.empty "dependencies" jsd.dependencies
                    |> writeUnlessDefaultWith E.list [] "enum" jsd.enum
                    |> writeUnlessDefaultWith jsonSchemaTypeOrTypes [] "type" jsd.``type``
                    |> writeUnlessDefaultWith jsonSchemaList [] "allOf" jsd.allOf
                    |> writeUnlessDefaultWith jsonSchemaList [] "anyOf" jsd.anyOf
                    |> writeUnlessDefaultWith jsonSchemaList [] "oneOf" jsd.oneOf
                    |> JO.writeOptionalWith jsonSchema "not" jsd.not
            JsonObject.buildWith objectWriter
        let jsonSchemaDefinition = jsonSchemaDefinitionDelayed ()
    module Decode =
        let inline (>=>) decoderA decoderB = Chiron.JsonResult.Operators.(>=>) decoderA decoderB
        let inline (>->) decoder mapper = Chiron.JsonResult.Operators.(>->) decoder mapper
        let inline (<!>) a2b aR = Chiron.ObjectReader.Operators.(<!>) a2b aR
        let inline (<*>) a2Rb aR = Chiron.ObjectReader.Operators.(<*>) a2Rb aR
        let uri =
            D.string >=> JsonResult.fromThrowingConverter (fun str -> System.Uri(str, System.UriKind.RelativeOrAbsolute))
        let regexFromString = JsonResult.fromThrowingConverter (fun str -> JsonRegex <| System.Text.RegularExpressions.Regex (str, System.Text.RegularExpressions.RegexOptions.ECMAScript))
        let regex =
            D.string >=> regexFromString
        let boundaryType =
            D.bool >-> BoundaryType.ofExclusiveBool
        let jsonSchemaType =
            D.string >=> (function
                | "object" -> JsonResult.Ok JsonSchemaType.Object
                | "array" -> JsonResult.Ok JsonSchemaType.Array
                | "string" -> JsonResult.Ok JsonSchemaType.String
                | "number" -> JsonResult.Ok JsonSchemaType.Number
                | "integer" -> JsonResult.Ok JsonSchemaType.Integer
                | "boolean" -> JsonResult.Ok JsonSchemaType.Boolean
                | "null" -> JsonResult.Ok JsonSchemaType.Null
                | _ -> JsonResult.deserializationError "Invalid JSON type; must be one of: object, array, number, integer, boolean, string, null")
        let jsonSchemaTypeOrTypes =
            D.oneOf
                [ D.listWith jsonSchemaType
                  jsonSchemaType >-> (fun s -> [s]) ]
        let jsonSchemaRef =
            JO.readWith uri "$ref"
            |> ObjectReader.toJsonReader
        let pair decodeA decodeB =
            fun (a, b) ->
                JsonResult.map D.mkTuple2 (decodeA a)
                |> JsonResult.applyDelay decodeB b
        let rec jsonSchema =
            let jsonSchemaDefinitionDelayed = fun x -> jsonSchemaDefinitionDelayed () x
            D.oneOf
                [ jsonSchemaRef >-> SchemaReference
                  jsonSchemaDefinitionDelayed >-> Schema ]
        and regexSchemaList =
            D.propertyListWithCustomKey regexFromString jsonSchema
        and additionalElements =
            D.oneOf
                [ D.bool >-> Allowed
                  jsonSchema >-> ElementSchema ]
        and arrayItemsSchema =
            D.oneOf
                [ jsonSchema >-> ItemsSchema
                  D.listWith jsonSchema >-> TupleSchema ]
        and dependencies =
            D.oneOf
                [ D.listWith D.string >-> DependentProperties
                  jsonSchema >-> DependentSchema ]
        and jsonSchemaDefinitionDelayed () =
            let readOrDefaultWith decode def key =
                (Option.defaultValue def)
                <!> JO.readOptionalWith decode key
            let toBoundaryType =
                Option.map (fun (m, mO) -> { limit = m; boundaryType = mO |> Option.defaultValue Inclusive })
            let readMaximum =
                toBoundaryType
                <!> JO.dependent "maximum" D.decimal "exclusiveMaximum" boundaryType
            let readMinimum =
                toBoundaryType
                <!> JO.dependent "minimum" D.decimal "exclusiveMinimum" boundaryType
            let makeJsonSchemaDefinition schemaId schema title description def multipleOf maximum minimum maxLen minLen pattern additionalItems items maxItems minItems uniqueItems maxProps minProps required additionalProps defs props patternProps deps enum memType anyOf allOf oneOf not =
                { id = schemaId
                  ``$schema`` = schema
                  title = title
                  description = description
                  ``default`` = def
                  multipleOf = multipleOf
                  maximum = maximum
                  minimum = minimum
                  maxLength = maxLen
                  minLength = minLen
                  pattern = pattern
                  additionalItems = additionalItems
                  items = items
                  maxItems = maxItems
                  minItems = minItems
                  uniqueItems = uniqueItems
                  maxProperties = maxProps
                  minProperties = minProps
                  required = required
                  additionalProperties = additionalProps
                  definitions = defs
                  properties = props
                  patternProperties = patternProps
                  dependencies = deps
                  enum = enum
                  ``type`` = memType
                  allOf = allOf
                  anyOf = anyOf
                  oneOf = oneOf
                  not = not }
            let objectReader =
                makeJsonSchemaDefinition
                <!> JO.readOptionalWith uri "id"
                <*> JO.readOptionalWith uri "$schema"
                <*> JO.readOptionalWith D.string "title"
                <*> JO.readOptionalWith D.string "description"
                <*> JO.readOptionalWith D.json "default"
                <*> JO.readOptionalWith D.decimal "multipleOf"
                <*> readMaximum
                <*> readMinimum
                <*> JO.readOptionalWith D.uint64 "maxLength"
                <*> readOrDefaultWith D.uint64 0UL "minLength"
                <*> JO.readOptionalWith regex "pattern"
                <*> readOrDefaultWith additionalElements AdditionalElements.empty "additionalItems"
                <*> JO.readOptionalWith arrayItemsSchema "items"
                <*> JO.readOptionalWith D.uint64 "maxItems"
                <*> readOrDefaultWith D.uint64 0UL "minItems"
                <*> readOrDefaultWith D.bool false "uniqueItems"
                <*> JO.readOptionalWith D.uint64 "maxProperties"
                <*> readOrDefaultWith D.uint64 0UL "minProperties"
                <*> readOrDefaultWith (D.listWith D.string) [] "required"
                <*> readOrDefaultWith additionalElements AdditionalElements.empty "additionalProperties"
                <*> readOrDefaultWith (D.mapWith jsonSchema) Map.empty "definitions"
                <*> readOrDefaultWith (D.mapWith jsonSchema) Map.empty "properties"
                <*> readOrDefaultWith regexSchemaList [] "patternProperties"
                <*> readOrDefaultWith (D.mapWith dependencies) Map.empty "dependencies"
                <*> readOrDefaultWith D.list [] "enum"
                <*> readOrDefaultWith jsonSchemaTypeOrTypes [] "type"
                <*> readOrDefaultWith (D.listWith jsonSchema) [] "allOf"
                <*> readOrDefaultWith (D.listWith jsonSchema) [] "anyOf"
                <*> readOrDefaultWith (D.listWith jsonSchema) [] "oneOf"
                <*> JO.readOptionalWith jsonSchema "not"
            ObjectReader.toJsonReader objectReader
        let jsonSchemaDefinition = jsonSchemaDefinitionDelayed ()

module E = Json.Encode
module D = Json.Decode

type BoundaryType with
    static member FromJson (_: BoundaryType) = D.boundaryType
    static member ToJson x = E.boundaryType x

type JsonSchemaType with
    static member FromJson (_: JsonSchemaType) = D.jsonSchemaType
    static member ToJson x = E.jsonSchemaType x

type JsonRegex with
    static member FromJson (_: JsonRegex) = D.regex
    static member ToJson x = E.regex x

type JsonSchema with
    static member FromJson (_: JsonSchema) = D.jsonSchema
    static member ToJson x = E.jsonSchema x

type ArrayItemsSchema with
    static member FromJson (_: ArrayItemsSchema) = D.arrayItemsSchema
    static member ToJson x = E.arrayItemsSchema x

type AdditionalElements with
    static member FromJson (_: AdditionalElements) = D.additionalElements
    static member ToJson x = E.additionalElements x

type Dependencies with
    static member FromJson (_:Dependencies) = D.dependencies
    static member ToJson x = E.dependencies x

type JsonSchemaDefinition with
    static member FromJson (_:JsonSchemaDefinition) = D.jsonSchemaDefinition
    static member ToJson x = E.jsonSchemaDefinition x

let jsonSchemaStr : string = loadJsonResourceAsString "swagger-schema"
let parsedJson = Json.parse jsonSchemaStr |> JsonResult.getOrThrow
let parsedSchema = D.jsonSchemaDefinition parsedJson |> JsonResult.getOrThrow

// printfn "%A" parsedSchema
// printfn "%s" (parsedSchema |> E.jsonSchemaDefinition |> Json.format)

open BenchmarkDotNet.Attributes

[<Config(typeof<CoreConfig>)>]
type SwaggerSchema() =
    [<Benchmark>]
    member __.Parse() =
        Json.parse jsonSchemaStr

    [<Benchmark>]
    member __.Decode() =
        D.jsonSchemaDefinition parsedJson

    [<Benchmark>]
    member __.ParseAndDecode() =
        Json.parse jsonSchemaStr
        |> JsonResult.bind D.jsonSchemaDefinition

    [<Benchmark>]
    member __.Format() =
        Json.format parsedJson

    [<Benchmark>]
    member __.Encode() =
        E.jsonSchemaDefinition parsedSchema

    [<Benchmark>]
    member __.EncodeAndFormat() =
        E.jsonSchemaDefinition parsedSchema
        |> Json.format

    [<Benchmark>]
    member __.RoundTrip() =
        Json.parse jsonSchemaStr
        |> JsonResult.bind D.jsonSchemaDefinition
        |> JsonResult.getOrThrow
        |> E.jsonSchemaDefinition
        |> Json.format
