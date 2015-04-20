module Chiron

open System
open System.Globalization
open System.Text
open Aether
open FParsec

(* RFC 7159

   Types, parsers, formatters and other utilities implemented to mirror the
   specification of JSON (JavaScript Object Notation Data Interchange Format)
   as defined in RFC 7159.

   Taken from [http://tools.ietf.org/html/rfc7159] *)

(* Types

   Simple AST for JSON, with included isomorphisms in Aether format for
   lens/isomorphism based modification of complex JSON structures. *)

type Json =
    | Array of Json list
    | Bool of bool
    | Number of float
    | Null of unit
    | Object of Map<string, Json>
    | String of string

    static member ArrayPIso : PIso<Json, Json list> =
        (function | Array x -> Some x
                  | _ -> None), Array

    static member BoolPIso : PIso<Json, bool> =
        (function | Bool x -> Some x
                  | _ -> None), Bool

    static member NumberPIso : PIso<Json, float> =
        (function | Number x -> Some x
                  | _ -> None), Number

    static member NullPIso : PIso<Json, unit> =
        (function | Null () -> Some ()
                  | _ -> None), Null

    static member ObjectPIso : PIso<Json, Map<string, Json>> =
        (function | Object x -> Some x
                  | _ -> None), Object

    static member StringPIso : PIso<Json, string> =
        (function | String x -> Some x
                  | _ -> None), String

(* Functional

   Functional signatures for working with Json types, implying a monadic
   approach to working with Json where appropriate.

   Additionally includes common functions for combining and creating
   functions of type Json<'a> which may be used via operator based
   combinators or a computation expression (both provided later). *)

[<AutoOpen>]
module Functional =

    type Json<'a> =
        Json -> JsonResult<'a> * Json

    and JsonResult<'a> =
        | Value of 'a
        | Error of string

    (* Functions

       Common functions for combining Json<'a> functions in to new
       forms, and for creating new Json<'a> functions given suitable
       initial data. *)

    [<RequireQualifiedAccess>]
    module Json =

        let inline init (a: 'a) : Json<'a> = 
            fun json ->
                Value a, json

        let inline error (e: string) : Json<'a> =
            fun json ->
                Error e, json

        let inline internal ofResult result =
            fun json ->
                result, json

        let inline bind (m: Json<'a>) (f: 'a -> Json<'b>) : Json<'b> =
            fun json ->
                match m json with
                | Value a, json -> (f a) json
                | Error e, json -> Error e, json

        let inline apply (f: Json<'a -> 'b>) (m: Json<'a>) : Json<'b> =
            bind f (fun f' ->
                bind m (fun m' ->
                    init (f' m')))

        let inline map (f: 'a -> 'b) (m: Json<'a>) : Json<'b> =
            bind m (fun m' ->
                init (f m'))

        let inline map2 (f: 'a -> 'b -> 'c) (m1: Json<'a>) (m2: Json<'b>) : Json<'c> =
            apply (apply (init f) m1) m2

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

[<AutoOpen>]
module Builder =

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

(* Lens

   Functional lens based access to nested Json data strcutures,
   using Aether format lenses. Uses Json<'a> based functions, so
   can be used monadicly. *)

[<AutoOpen>]
module Lens =

    (* Functions *)

    [<RequireQualifiedAccess>]
    module Json =

        let getLens l : Json<_> =
            fun json ->
                Value (Lens.get l json), json

        let getLensPartial l : Json<_> =
            fun json ->
                match Lens.getPartial l json with
                | Some x -> Value x, json
                | _ -> Error (sprintf "couldn't use lens %A on json '%A'" l json), json

        let tryGetLensPartial l : Json<_> =
            fun json ->
                Value (Lens.getPartial l json), json

        let setLens l v : Json<_> =
            fun json ->
                Value (), Lens.set l v json

        let setLensPartial l v : Json<_> =
            fun json ->
                Value (), Lens.setPartial l v json

        let mapLens l f : Json<_> =
            fun json ->
                Value (), Lens.map l f json

        let mapLensPartial l f : Json<_> =
            fun json ->
                Value (), Lens.mapPartial l f json

(* Escaping

   Functions for escaped string parsing and formatting, as a
   minimal encoding function (escaping only disallowed codepoints,
   but normalizing any input). *)

[<RequireQualifiedAccess>]
module internal Escaping =

    let private digit i =
        (i >= 0x30 && i <= 0x39)

    let private hexdig i =
        (digit i)
     || (i >= 0x41 && i <= 0x46)
     || (i >= 0x61 && i <= 0x66)

    let private unescaped i =
            i >= 0x20 && i <= 0x21
         || i >= 0x23 && i <= 0x5b
         || i >= 0x5d && i <= 0x10ffff

    let private unescapedP =
        satisfy (int >> unescaped)

    let private hexdig4P =
        manyMinMaxSatisfy 4 4 (int >> hexdig)
        |>> fun s ->
            char (Int32.Parse (s, NumberStyles.HexNumber))

    let private escapedP =
            skipChar '\\'
        >>. choice [
                pchar '"'
                pchar '\\'
                pchar '/'
                skipChar 'b' >>% '\u0008'
                skipChar 'f' >>% '\u000c'
                skipChar 'n' >>% '\u000a'
                skipChar 'r' >>% '\u000d'
                skipChar 't' >>% '\u0009'
                skipChar 'u' >>. hexdig4P ]

    let private charP =
        choice [
            unescapedP
            escapedP ]

    let parse =
        many charP

    let escape (s: string) =
        let rec escape r =
            function | [] -> r
                     | h :: t when (unescaped (int h)) ->
                        escape (r @ [ h ]) t
                     | h :: t ->
                        let n =
                            match h with
                            | '"' -> [ '\\'; '"' ]
                            | '\\' -> [ '\\'; '\\' ]
                            | '\b' -> [ '\\'; 'b' ]
                            | '\f' -> [ '\\'; 'f' ]
                            | '\n' -> [ '\\'; 'n' ]
                            | '\r' -> [ '\\'; 'r' ]
                            | '\t' -> [ '\\'; 't' ]
                            | x -> [ '\\'; 'u' ] @ [ for c in ((int x).ToString ("X4")) -> c ]

                        escape (r @ n) t

        new string (List.toArray (escape [] [ for c in s -> c ]))

(* Parsing

   Functions for parsing string JSON data to Json types, using
   FParsec.

   Functions parse and tryParse are effectively static,
   while import parses the provided string JSON and replaces the
   current state of a Json<'a> function. *)

[<AutoOpen>]
module Parsing =

    (* Helpers

       Utlility functions for working with intermediate states of
       parsers, minimizing boilerplate and unpleasant code. *)

    let private emp =
        function | Some x -> x
                 | _ -> ""

    (* Grammar

       Common grammatical elements forming parts of other parsers as
       as defined in RFC 1759. The elements are implemented slightly
       differently due to the design of parser combinators used, chiefly
       concerning whitespace, which is always implemented as trailing.

       Taken from RFC 7159, Section 2 Grammar
       See [http://tools.ietf.org/html/rfc7159#section-2] *)

    let private wsp i =
            i = 0x20
         || i = 0x09
         || i = 0x0a
         || i = 0x0d

    let private wspP =
        skipManySatisfy (int >> wsp)

    let private charWspP c =
        skipChar c .>> wspP

    let private beginArrayP =
        charWspP '['

    let private beginObjectP =
        charWspP '{'

    let private endArrayP =
        charWspP ']'

    let private endObjectP =
        charWspP '}'

    let private nameSeparatorP =
        charWspP ':'

    let private valueSeparatorP =
        charWspP ','

    (* JSON

       As the JSON grammar is recursive in various forms, we create a
       reference parser which will be assigned later, allowing for recursive
       definition of parsing rules. *)

    let private jsonP, jsonR =
        createParserForwardedToRef ()

    (* Values

       Taken from RFC 7159, Section 3 Values
       See [http://tools.ietf.org/html/rfc7159#section-3] *)

    let private boolP =
            stringReturn "true" true
        <|> stringReturn "false" false
        .>> wspP

    let private nullP =
        stringReturn "null" () .>> wspP

    (* Numbers

       The numbers parser is implemented by parsing the JSON number value
       in to a known representation valid as string under Double.Parse
       natively (invoked as the float conversion function on the eventual
       string).

       Taken from RFC 7159, Section 6 Numbers
       See [http://tools.ietf.org/html/rfc7159#section-6] *)

    let private digit1to9 i =
            i >= 0x31 && i <= 0x39

    let private digit i =
            digit1to9 i
         || i = 0x30

    let private e i =
            i = 0x45 
         || i = 0x65

    let private minusP =
        charReturn '-' "-"

    let private intP =
        charReturn '0' "0" <|> (satisfy (int >> digit1to9) .>>. manySatisfy (int >> digit)
        |>> fun (h, t) -> string h + t)

    let private fracP =
        skipChar '.' >>.  many1Satisfy (int >> digit)
        |>> fun i -> "." + i

    let private expP =
            skipSatisfy (int >> e)
        >>. opt (charReturn '-' "-" <|> charReturn '+' "+")
        .>>. many1Satisfy (int >> digit)
        |>> function | Some s, d -> "e" + s + d
                     | _, d -> "e" + d

    let private numberP =
        pipe4 (opt minusP) intP (opt fracP) (opt expP) (fun m i f e ->
            float (emp m + i + emp f + emp e)) .>> wspP

    (* Strings

       Taken from RFC 7159, Section 7 Strings
       See [http://tools.ietf.org/html/rfc7159#section-7] *)

    let private quotationMarkP =
        skipChar '"'

    let private stringP =
        between quotationMarkP quotationMarkP Escaping.parse .>> wspP
        |>> fun cs -> new string (List.toArray cs)

    (* Objects

       Taken from RFC 7159, Section 4 Objects
       See [http://tools.ietf.org/html/rfc7159#section-4] *)

    let private memberP =
        stringP .>> nameSeparatorP .>>. jsonP

    let private objectP =
        between beginObjectP endObjectP (sepBy memberP valueSeparatorP)
        |>> Map.ofList

    (* Arrays

       Taken from RFC 7159, Section 5 Arrays
       See [http://tools.ietf.org/html/rfc7159#section-5] *)

    let private arrayP =
        between beginArrayP endArrayP (sepBy jsonP valueSeparatorP)

    (* JSON *)

    do jsonR :=
            wspP
        >>. choice [
                arrayP  |>> Array
                boolP   |>> Bool
                nullP   |>> Null
                numberP |>> Number
                objectP |>> Object
                stringP |>> String ]

    (* Functions

       Functions for parsing (or attempting to parse) JSON data as strings,
       returning data as Json types when successful (or as a wrapped Json type
       in the case of attempt-based parsing). *)

    [<RequireQualifiedAccess>]
    module Json =

        let internal parseJson s =
            match run jsonP s with
            | Success (json, _, _) -> Value json
            | Failure (e, _, _) -> Error e

        let tryParse =
               parseJson
            >> function | Value json -> Some json
                        | _ -> None

        let parse =
               parseJson
            >> function | Value json -> json
                        | Error e -> failwith e

        let import s =
            fun json ->
                match parseJson s with
                | Value json -> Value (), json
                | Error e -> Error e, json

(* Formatting *)

[<AutoOpen>]
module Formatting =

    (* Helpers *)

    type private Formatter<'a> =
        'a -> StringBuilder -> StringBuilder

    type private Separator =
        StringBuilder -> StringBuilder

    let private append (s: string) (b: StringBuilder) =
        b.Append s

    let private appendf (s: string) (v1: obj) (b: StringBuilder) =
        b.AppendFormat (s, v1)

    let private join<'a> (f: Formatter<'a>) (s: Separator) =
        let rec join values (b: StringBuilder) =
            match values with
            | [] -> b
            | h :: [] -> f h b
            | h :: t -> (f h >> s >> join t) b

        join

    (* Formatters *)

    let rec private formatJson =
        function | Array x -> formatArray x
                 | Bool x -> formatBool x
                 | Number x -> formatNumber x
                 | Null _ -> formatNull ()
                 | Object x -> formatObject x
                 | String x -> formatString x

    and private formatArray =
        function | x ->
                       append "[" 
                    >> join formatJson (append ",") x 
                    >> append "]"

    and private formatBool =
        function | true -> append "true"
                 | _ -> append "false"

    and private formatNumber =
        function | x -> append (string x)

    and private formatNull =
        function | () -> append "null"

    and private formatObject =
        function | x -> 
                       append "{" 
                    >> join (fun (k, v) -> appendf "\"{0}\":" k >> formatJson v)
                            (append ",")
                            (Map.toList x) 
                    >> append "}"

    and private formatString =
        function | x -> appendf "\"{0}\"" (Escaping.escape x)

    (* Functions *)

    [<RequireQualifiedAccess>]
    module Json =

        let format json =
            StringBuilder ()
            |> formatJson json
            |> string

(* Mapping

   Functional mapping between Json and native F# data structures,
   through statically inferred types. Types providing FromJson and
   ToJson static members with appropriate signatures can be
   seamlessly serialized and deserialized.

   This approach is the same as that taken by the Fleece library,
   credit for which is due to Mauricio Scheffer. *)

[<AutoOpen>]
module Mapping =

    open Operators

    (* Lenses

       Common lenses and lens functions for mapping. *)

    let inline arrayPLens _ =
        idLens <-?> Json.ArrayPIso

    let inline boolPLens _ =
        idLens <-?> Json.BoolPIso

    let inline numberPLens _ =
        idLens <-?> Json.NumberPIso

    let inline objectPLens _ =
        idLens <-?> Json.ObjectPIso

    let inline objectKeyPLens key =
        objectPLens () >??> mapPLens key

    let inline stringPLens _ =
        idLens <-?> Json.StringPIso

    let (<%>) (l : PLens<Json, 'a>) (f, fMsg) : Json<'b> =
      fun x ->
          match Json.getLensPartial l x with
          | Value v, json -> Value (f v), json
          | Error e, json -> Error (fMsg json), json

    let (<%%>) (m : Json<'a>) (l : PLens<Json, 'b>, (f : 'b -> 'a), fMsg) : Json<'a> =
        fun x ->
            match m x with
            | Error e, json -> (l <%> (f, fMsg)) json
            | Value v, json -> Value v, json

    (* From

       Default conversion functions (static members on FromJsonDefaults)
       and statically inferred inline conversion functions for conversion
       from Json to F# data structures. *)

    (* Defaults *)

    type FromJsonDefaults = FromJsonDefaults with

        (* Basic Types *)

        static member inline FromJson (_: bool) =
            Json.getLensPartial (boolPLens ())

        static member inline FromJson (_: decimal) : Json<decimal> =
          numberPLens ()
          <%> (decimal, sprintf "parsing number %A to decimal failed")
          <%%> (stringPLens(), decimal, sprintf "parsing string %A to decimal failed")

        static member inline FromJson (_: float) =
            Json.getLensPartial (numberPLens ())

        static member inline FromJson (_: int) =
            int <!> Json.getLensPartial (numberPLens ())

        static member inline FromJson (_: int16) =
            int16 <!> Json.getLensPartial (numberPLens ())

        static member inline FromJson (_: int64) =
            int64 <!> Json.getLensPartial (numberPLens ())

        static member inline FromJson (_: single) =
            single <!> Json.getLensPartial (numberPLens ())

        static member inline FromJson (_: string) =
            Json.getLensPartial (stringPLens ())

        static member inline FromJson (_: uint16) =
            uint16 <!> Json.getLensPartial (numberPLens ())

        static member inline FromJson (_: uint32) =
            uint32 <!> Json.getLensPartial (numberPLens ())

        static member inline FromJson (_: uint64) =
            uint64 <!> Json.getLensPartial (numberPLens ())

        (* Common Types *)

        static member inline FromJson (_: DateTime) =
                fun x ->
                    match DateTime.TryParseExact (x, [| "s"; "r"; "o" |], null, DateTimeStyles.AdjustToUniversal) with
                    | true, x -> Json.init x
                    | _ -> Json.error "datetime"
            =<< Json.getLensPartial (stringPLens ())

        static member inline FromJson (_: DateTimeOffset) =
                fun x ->
                    match DateTimeOffset.TryParseExact (x, [|"yyyy-MM-dd'T'HH:mm:ss.FFFK" |], null, DateTimeStyles.None) with
                    | true, x -> Json.init x
                    | _ -> Json.error "datetimeoffset"
            =<< Json.getLensPartial (stringPLens ())

        static member inline FromJson (_: Guid) =
                fun x ->
                    match Guid.TryParse x with
                    | true, x -> Json.init x
                    | _ -> Json.error "guid"
            =<< Json.getLensPartial (stringPLens ())

    (* Mapping Functions

       Functions for applying the FromJson function to Json to produce
       new instances of 'a where possible, including folding the FromJson
       function across a list of Json objects. *)

    let inline internal fromJsonDefaults (a: ^a, _: ^b) =
        ((^a or ^b) : (static member FromJson: ^a -> ^a Json) a)

    let inline internal fromJson x =
        fst (fromJsonDefaults (Unchecked.defaultof<'a>, FromJsonDefaults) x)

    let inline internal fromJsonFold init fold xs =
        List.fold (fun r x ->
            match r with
            | Error e ->
                Error e
            | Value xs ->
                match fromJson x with
                | Value x -> Value (fold x xs)
                | Error e -> Error e) (Value init) (List.rev xs)

    (* Defaults *)

    type FromJsonDefaults with

        (* Arrays *)

        static member inline FromJson (_: 'a array) : Json<'a array> =
                fromJsonFold Array.empty (fun x xs -> Array.append [| x |] xs) >> Json.ofResult
            =<< Json.getLensPartial (arrayPLens ())

        (* Lists *)

        static member inline FromJson (_: 'a list) : Json<'a list> =
                fromJsonFold List.empty (fun x xs -> x :: xs) >> Json.ofResult
            =<< Json.getLensPartial (arrayPLens ())

        (* Maps *)

        static member inline FromJson (_: Map<string,'a>) : Json<Map<string,'a>> =
                fun x ->
                    let k, v = (Map.toList >> List.unzip) x
                    List.zip k >> Map.ofList <!> Json.ofResult (fromJsonFold [] (fun x xs -> x :: xs) v)
            =<< Json.getLensPartial (objectPLens ())

        (* Sets *)

        static member inline FromJson (_: Set<'a>) : Json<Set<'a>> =
                fromJsonFold Set.empty Set.add >> Json.ofResult
            =<< Json.getLensPartial (arrayPLens ())

        (* Options *)

        static member inline FromJson (_: 'a option) : Json<'a option> =
                function | Null _ -> Json.init None
                         | x -> Some <!> Json.ofResult (fromJson x)
            =<< Json.getLens idLens

        (* Tuples *)

        static member inline FromJson (_: 'a * 'b) : Json<'a * 'b> =
                function | a :: b :: [] ->
                                fun a b -> a, b
                            <!> Json.ofResult (fromJson a)
                            <*> Json.ofResult (fromJson b)
                         | _ ->
                            Json.error "tuple2"
            =<< Json.getLensPartial (arrayPLens ())

        static member inline FromJson (_: 'a * 'b * 'c) : Json<'a * 'b * 'c> =
                function | a :: b :: c :: [] ->
                                fun a b c -> a, b, c
                            <!> Json.ofResult (fromJson a)
                            <*> Json.ofResult (fromJson b)
                            <*> Json.ofResult (fromJson c)
                         | _ ->
                            Json.error "tuple3"
            =<< Json.getLensPartial (arrayPLens ())

    (* To
    
        *)

    (* Defaults *)

    type ToJsonDefaults = ToJsonDefaults with

        (* Basic Types *)

        static member inline ToJson (x: bool) =
            Json.setLensPartial (boolPLens ()) x

        static member inline ToJson (x: decimal) =
            Json.setLensPartial (stringPLens ()) (x.ToString(CultureInfo.InvariantCulture))

        static member inline ToJson (x: float) =
            Json.setLensPartial (numberPLens ()) x

        static member inline ToJson (x: int) =
            Json.setLensPartial (numberPLens ()) (float x)

        static member inline ToJson (x: int16) =
            Json.setLensPartial (numberPLens ()) (float x)

        static member inline ToJson (x: int64) =
            Json.setLensPartial (numberPLens ()) (float x)

        static member inline ToJson (x: single) =
            Json.setLensPartial (numberPLens ()) (float x)

        static member inline ToJson (x: string) =
            Json.setLensPartial (stringPLens ()) x

        static member inline ToJson (x: uint16) =
            Json.setLensPartial (numberPLens ()) (float x)

        static member inline ToJson (x: uint32) =
            Json.setLensPartial (numberPLens ()) (float x)

        static member inline ToJson (x: uint64) =
            Json.setLensPartial (numberPLens ()) (float x)

        (* Common Types *)

        static member inline ToJson (x: DateTime) =
            Json.setLensPartial (stringPLens ()) (x.ToUniversalTime().ToString("o"))
        
        static member inline ToJson (x: DateTimeOffset) =
            Json.setLensPartial (stringPLens ()) (x.ToString("o"))

        static member inline ToJson (x: Guid) =
            Json.setLensPartial (stringPLens ()) (string x)

    (* Mapping Functions

       Functions for applying the ToJson function to data structures to produce
       new Json instances. *)

    let inline internal toJsonDefaults (a: ^a, _: ^b) =
        ((^a or ^b) : (static member ToJson: ^a -> unit Json) a)

    let inline internal toJson (x: 'a) =
        snd (toJsonDefaults (x, ToJsonDefaults) (Object (Map.empty)))

    (* Defaults *)

    type ToJsonDefaults with

        (* Arrays *)

        static member inline ToJson (x: 'a array) =
            Json.setLens idLens (Array ((Array.toList >> List.map toJson) x))

        (* Lists *)

        static member inline ToJson (x: 'a list) =
            Json.setLens idLens (Array (List.map toJson x))

        (* Maps *)

        static member inline ToJson (x: Map<string,'a>) =
            Json.setLens idLens (Object (Map.map (fun _ a -> toJson a) x))

        (* Options *)

        static member inline ToJson (x: 'a option) =
            Json.setLens idLens ((function | Some a -> toJson a 
                                           | _ -> Null ()) x)

        (* Sets *)

        static member inline ToJson (x: Set<'a>) =
            Json.setLens idLens (Array ((Set.toList >> List.map toJson) x))

        (* Tuples *)

        static member inline ToJson ((a, b)) =
            Json.setLens idLens (Array [ toJson a; toJson b ])

        static member inline ToJson ((a, b, c)) =
            Json.setLens idLens (Array [ toJson a; toJson b; toJson c ])

    (* Functions

        *)

    [<RequireQualifiedAccess>]
    module Json =

        (* Read *)

        let inline read key =
                fromJson >> Json.ofResult
            =<< Json.getLensPartial (objectKeyPLens key) 

        let inline tryRead key =
                function | Some json -> Some <!> Json.ofResult (fromJson json)
                         | _ -> Json.init None
            =<< Json.tryGetLensPartial (objectKeyPLens key)

        (* Write *)

        let inline write key value =
            Json.setLensPartial (objectKeyPLens key) (toJson value)

        (* Deserialization *)

        let inline deserialize json =
            fromJson json
            |> function | Value a -> a
                        | Error e -> failwith e

        let inline tryDeserialize json =
            fromJson json
            |> function | Value a -> Some a
                        | _ -> None

        (* Serialization *)

        let inline serialize a =
            toJson a
