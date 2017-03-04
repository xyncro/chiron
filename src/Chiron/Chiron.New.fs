module ChironN.NewParser

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

type JsonNumber = JsonNumber of stringValue:string

type JsonObject =
    | UnmappedObject of propList:(string * Json) list
    | MappedObject of propList:(string * Json) list * propMap:Map<string,Json>

and Json =
    | Array of elements:Json list
    | Bool of bool
    | Null
    | Number of JsonNumber
    | Object of properties:JsonObject
    | String of string

    (* Epimorphisms *)

    static member internal Array__ =
        (function | Array x -> Some x
                  | _ -> None), Array

    static member internal Bool__ =
        (function | Bool x -> Some x
                  | _ -> None), Bool

    static member internal Null__ =
        (function | Null -> Some ()
                  | _ -> None), (fun () -> Null)

    static member internal Number__ =
        (function | Number x -> Some x
                  | _ -> None), Number

    static member internal Object__ =
        (function | Object x -> Some x
                  | _ -> None), Object

    static member internal String__ =
        (function | String x -> Some x
                  | _ -> None), String

    (* Prisms *)

    static member Array_ =
        Prism.ofEpimorphism Json.Array__

    static member Bool_ =
        Prism.ofEpimorphism Json.Bool__

    static member Null_ =
        Prism.ofEpimorphism Json.Null__

    static member Number_ =
        Prism.ofEpimorphism Json.Number__

    static member Object_ =
        Prism.ofEpimorphism Json.Object__

    static member String_ =
        Prism.ofEpimorphism Json.String__

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonObject =
    let empty = UnmappedObject []

(* Functional

   Functional signatures for working with Json types, implying a monadic
   approach to working with Json where appropriate.

   Additionally includes common functions for combining and creating
   functions of type Json<'a> which may be used via operator based
   combinators or a computation expression (both provided later). *)

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

type Json<'a> =
    Json -> Result<'a,JsonFailure list> * Json

type Deserializer<'a> =
    Json -> Result<'a,JsonFailure list>

type Serializer =
    Json -> Json

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

[<RequireQualifiedAccess>]
module Serializer =
    let toJson (aS : Serializer) : Json<'a> =
        fun json ->
            (Ok (), aS json)



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
        let pjnum : Parser<JsonNumber,'u> =
            fun stream ->
                let reply = numberLiteralE jsonNumOpts (ErrorMessageList(ErrorMessage.Expected("JSON number"))) stream
                if reply.Status = ReplyStatus.Ok then
                    Reply(JsonNumber reply.Result.String)
                else
                    Reply(reply.Status, reply.Error)

        let jnumber = pjnum |>> Json.Number

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

    let inline append (s: string) (b: StringBuilder) =
        b.Append s

    let inline appendf (s: string) (v1: obj) (b: StringBuilder) =
        b.AppendFormat (s, v1)

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

    let rec writeString (cs:string) (index:int) (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        if index >= (String.length cs - 1) then sb
        else
            let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
            //let nextEscapeIndex = System.Array.FindIndex(cs, int index, isEscapeCharPred)
            if nextEscapeIndex = -1 then
                sb.Append(cs,index,String.length cs - index)
            else if nextEscapeIndex = index then
                sb.Append(escaped cs.[nextEscapeIndex])
                |> writeString cs (nextEscapeIndex + 1)
            else
                sb.Append(cs,index,nextEscapeIndex - index)
                sb.Append(escaped cs.[nextEscapeIndex])
                |> writeString cs (nextEscapeIndex + 1)

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

    // let rec joinRev (f: Formatter<'a>) (sep: string) values (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
    //     match values with
    //     | [] -> sb
    //     | [v] -> f v tw
    //     | v :: vs ->
    //         f v sb
    //         |> s
    //         |> join f s vs

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
        | Json.Number (JsonNumber str) -> agg + (String.length str |> uint32)
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

    let addPropertyNameSeparator { PropertyNameSpacing = pns } sb =
        match pns with
        | NoSpaceBetweenNameAndValue -> append ":" sb
        | SpaceBetweenNameAndValue -> append ": " sb

    let rec formatJson level options x sb =
        match x with
        | Json.Array elems -> formatArray level options elems sb
        | Json.Bool x -> formatBool x sb
        | Json.Number x -> formatNumber x sb
        | Json.Null -> formatNull sb
        | Json.Object (UnmappedObject o) -> formatObject level options o sb
        | Json.Object (MappedObject (o,_)) -> formatObject level options o sb
        | Json.String s -> formatString s sb

    and formatArray level options elems sb =
        let newLevel = level + 1u
        let prefix = elementSeparator newLevel options
        let separator = "," + prefix
        let suffix = elementSeparator level options
        append "[" sb
        |> addSeparation options prefix
        |> join (formatJson newLevel options) separator elems
        |> addSeparation options suffix
        |> append "]"

    and formatBool b sb =
        let str =
            if b then "true" else "false"
        append str sb

    and formatNumber (JsonNumber n) sb =
        append n sb

    and formatNull sb =
        append "null" sb

    and formatProperty level options (k,v) sb =
        formatString k sb
        |> addPropertyNameSeparator options
        |> formatJson level options v

    and formatObject level options props sb =
        let newLevel = level + 1u
        let prefix = elementSeparator newLevel options
        let separator = "," + prefix
        let suffix = elementSeparator level options
        append "{" sb
        |> addSeparation options prefix
        |> join (fun p sb -> formatProperty newLevel options p sb) separator props
        |> addSeparation options suffix
        |> append "}"

    and formatString str sb =
        append "\"" sb
        |> writeString str 0
        |> append "\""

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
