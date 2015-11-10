module Chiron

open System
open System.Globalization
open System.Text
open Aether
open Aether.Operators
open FParsec

(* RFC 7159

   Types, parsers, formatters and other utilities implemented to mirror the
   specification of JSON (JavaScript Object Notation Data Interchange Format)
   as defined in RFC 7159.

   Taken from [http://tools.ietf.org/html/rfc7159] *)

(* Types

   Simple AST for JSON, with included isomorphisms and lenses in Aether form for
   lens/isomorphism based modification of complex JSON structures. *)

type Json =
    | Array of Json list
    | Bool of bool
    | Null of unit
    | Number of decimal
    | Object of Map<string, Json>
    | String of string

    (* Epimorphisms *)

    static member internal Array__ =
        (function | Array x -> Some x
                  | _ -> None), Array

    static member internal Bool__ =
        (function | Bool x -> Some x
                  | _ -> None), Bool

    static member internal Null__ =
        (function | Null () -> Some ()
                  | _ -> None), Null

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
        id_ <-?> Json.Array__

    static member Bool_ =
        id_ <-?> Json.Bool__

    static member Null_ =
        id_ <-?> Json.Null__

    static member Number_ =
        id_ <-?> Json.Number__

    static member Object_ =
        id_ <-?> Json.Object__

    static member String_ =
        id_ <-?> Json.String__

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

(* Optics

   Functional optics based access to nested Json data structures,
   using Aether format lenses/prisms/etc. Uses Json<'a> based functions, so
   can be used monadically. *)

[<AutoOpen>]
module Optics =

    (* Functions *)

    [<RequireQualifiedAccess>]
    module Json =

        [<RequireQualifiedAccess>]
        module Lens =

            let get l : Json<_> =
                fun json ->
                    Value (Lens.get l json), json

            let set l v : Json<_> =
                fun json ->
                    Value (), Lens.set l v json

            let map l f : Json<_> =
                fun json ->
                    Value (), Lens.map l f json

        [<RequireQualifiedAccess>]
        module Prism =

            let get p : Json<_> =
                fun json ->
                    match Prism.get p json with
                    | Some x -> Value x, json
                    | _ -> Error (sprintf "Couldn't use Prism %A on JSON: '%A'." p json), json

            let tryGet p : Json<_> =
                fun json ->
                    Value (Prism.get p json), json

            let set p v : Json<_> =
                fun json ->
                    Value (), Prism.set p v json

            let map p f : Json<_> =
                fun json ->
                    Value (), Prism.map p f json

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
            decimal (emp m + i + emp f + emp e)) .>> wspP

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
            >> function | Value json -> Choice1Of2 json
                        | Error e -> Choice2Of2 e

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
            | [v] -> f v b
            | v :: vs -> (f v >> s >> join vs) b

        join

    (* Options

       Options for formatting, defined as functions for spacing and newline
       formatting appenders. Predefined formats are given as static members
       as a shorthand. *)

    type JsonFormattingOptions =
      { Spacing : StringBuilder -> StringBuilder
        NewLine : int -> StringBuilder -> StringBuilder }

      static member Compact =
        { Spacing = id
          NewLine = fun _ x -> x }

      static member SingleLine =
        { Spacing = append " "
          NewLine = fun _ x -> x }

      static member Pretty =
        { Spacing = append " "
          NewLine = fun level -> append "\n" >> append (String.replicate level "  ") }

    (* Formatters *)

    let rec private formatJson level options =
        function | Array x -> formatArray level options x
                 | Bool x -> formatBool x
                 | Number x -> formatNumber x
                 | Null _ -> formatNull ()
                 | Object x -> formatObject level options x
                 | String x -> formatString x

    and private formatArray level options =
        function | x ->
                       append "["
                    >> options.NewLine (level + 1)
                    >> join (formatJson (level + 1) options) (append "," >> options.NewLine (level + 1)) x
                    >> options.NewLine level
                    >> append "]"

    and private formatBool =
        function | true -> append "true"
                 | _ -> append "false"

    and private formatNumber =
        function | x -> append (string x)

    and private formatNull =
        function | () -> append "null"

    and private formatObject level options =
        function | x -> 
                       append "{" 
                    >> options.NewLine (level + 1)
                    >> join (fun (k, v) -> appendf "\"{0}\":" (Escaping.escape k) >> options.Spacing >> formatJson (level + 1) options v)
                            (append "," >> options.NewLine (level + 1))
                            (Map.toList x)
                    >> options.NewLine level
                    >> append "}"

    and private formatString =
        function | x -> appendf "\"{0}\"" (Escaping.escape x)

    (* Functions *)

    [<RequireQualifiedAccess>]
    module Json =

        let format json =
            StringBuilder ()
            |> formatJson 0 JsonFormattingOptions.Compact json
            |> string

        let formatWith options json =
            StringBuilder ()
            |> formatJson 0 options json
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

    (* From

       Default conversion functions (static members on FromJsonDefaults)
       and statically inferred inline conversion functions for conversion
       from Json to F# data structures. *)

    (* Defaults *)

    type FromJsonDefaults = FromJsonDefaults with

        (* Basic Types *)

        static member inline FromJson (_: bool) =
            Json.Prism.get Json.Bool_

        static member inline FromJson (_: decimal) =
            id <!> Json.Prism.get Json.Number_

        static member inline FromJson (_: float) =
            float <!> Json.Prism.get Json.Number_

        static member inline FromJson (_: int) =
            int <!> Json.Prism.get Json.Number_

        static member inline FromJson (_: int16) =
            int16 <!> Json.Prism.get Json.Number_

        static member inline FromJson (_: int64) =
            int64 <!> Json.Prism.get Json.Number_

        static member inline FromJson (_: single) =
            single <!> Json.Prism.get Json.Number_

        static member inline FromJson (_: string) =
            Json.Prism.get Json.String_

        static member inline FromJson (_: uint16) =
            uint16 <!> Json.Prism.get Json.Number_

        static member inline FromJson (_: uint32) =
            uint32 <!> Json.Prism.get Json.Number_

        static member inline FromJson (_: uint64) =
            uint64 <!> Json.Prism.get Json.Number_

        (* Common Types *)

        static member inline FromJson (_: DateTime) =
                fun x ->
                    match DateTime.TryParseExact (x, [| "s"; "r"; "o" |], null, DateTimeStyles.AdjustToUniversal) with
                    | true, x -> Json.init x
                    | _ -> Json.error "datetime"
            =<< Json.Prism.get Json.String_

        static member inline FromJson (_: DateTimeOffset) =
                fun x ->
                    match DateTimeOffset.TryParseExact (x, [|"yyyy-MM-dd'T'HH:mm:ss.FFFK" |], null, DateTimeStyles.None) with
                    | true, x -> Json.init x
                    | _ -> Json.error "datetimeoffset"
            =<< Json.Prism.get Json.String_

        static member inline FromJson (_: Guid) =
                fun x ->
                    match Guid.TryParse x with
                    | true, x -> Json.init x
                    | _ -> Json.error "guid"
            =<< Json.Prism.get Json.String_

        (* Json Type *)

        static member inline FromJson (_: Json) =
            Json.Lens.get id_

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
                fromJsonFold [||] (fun x xs -> Array.append [| x |] xs) >> Json.ofResult
            =<< Json.Prism.get Json.Array_

        (* Lists *)

        static member inline FromJson (_: 'a list) : Json<'a list> =
                fromJsonFold [] (fun x xs -> x :: xs) >> Json.ofResult
            =<< Json.Prism.get Json.Array_

        (* Maps *)

        static member inline FromJson (_: Map<string,'a>) : Json<Map<string,'a>> =
                fun x ->
                    let k, v = (Map.toList >> List.unzip) x
                    List.zip k >> Map.ofList <!> Json.ofResult (fromJsonFold [] (fun x xs -> x :: xs) v)
            =<< Json.Prism.get Json.Object_

        (* Sets *)

        static member inline FromJson (_: Set<'a>) : Json<Set<'a>> =
                fromJsonFold Set.empty Set.add >> Json.ofResult
            =<< Json.Prism.get Json.Array_

        (* Options *)

        static member inline FromJson (_: 'a option) : Json<'a option> =
                function | Null _ -> Json.init None
                         | x -> Some <!> Json.ofResult (fromJson x)
            =<< Json.Lens.get id_

        (* Tuples *)

        static member inline FromJson (_: 'a * 'b) : Json<'a * 'b> =
                function | a :: [b] ->
                                fun a b -> a, b
                            <!> Json.ofResult (fromJson a)
                            <*> Json.ofResult (fromJson b)
                         | _ ->
                            Json.error "tuple2"
            =<< Json.Prism.get Json.Array_

        static member inline FromJson (_: 'a * 'b * 'c) : Json<'a * 'b * 'c> =
                function | a :: b :: [c] ->
                                fun a b c -> a, b, c
                            <!> Json.ofResult (fromJson a)
                            <*> Json.ofResult (fromJson b)
                            <*> Json.ofResult (fromJson c)
                         | _ ->
                            Json.error "tuple3"
            =<< Json.Prism.get Json.Array_

    (* To
    
        *)

    (* Defaults *)

    type ToJsonDefaults = ToJsonDefaults with

        (* Basic Types *)

        static member inline ToJson (x: bool) =
            Json.Prism.set Json.Bool_ x

        static member inline ToJson (x: decimal) =
            Json.Prism.set Json.Number_ x

        static member inline ToJson (x: float) =
            match x with
            | x when Double.IsInfinity x -> failwith "Serialization of Infinite Numbers Invalid."
            | x when Double.IsNaN x -> failwith "Serialization of NaN Invalid."
            | x -> Json.Prism.set Json.Number_ (decimal x)

        static member inline ToJson (x: int) =
            Json.Prism.set Json.Number_ (decimal x)

        static member inline ToJson (x: int16) =
            Json.Prism.set Json.Number_ (decimal x)

        static member inline ToJson (x: int64) =
            Json.Prism.set Json.Number_ (decimal x)

        static member inline ToJson (x: single) =
            Json.Prism.set Json.Number_ (decimal x)

        static member inline ToJson (x: string) =
            Json.Prism.set Json.String_ x

        static member inline ToJson (x: uint16) =
            Json.Prism.set Json.Number_ (decimal x)

        static member inline ToJson (x: uint32) =
            Json.Prism.set Json.Number_ (decimal x)

        static member inline ToJson (x: uint64) =
            Json.Prism.set Json.Number_ (decimal x)

        (* Common Types *)

        static member inline ToJson (x: DateTime) =
            Json.Prism.set Json.String_ (x.ToUniversalTime().ToString("o"))
        
        static member inline ToJson (x: DateTimeOffset) =
            Json.Prism.set Json.String_ (x.ToString("o"))

        static member inline ToJson (x: Guid) =
            Json.Prism.set Json.String_ (string x)

        (* Json Type *)

        static member inline ToJson (x: Json) =
            Json.Lens.set id_ x

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
            Json.Lens.set id_ (Array ((Array.toList >> List.map toJson) x))

        (* Lists *)

        static member inline ToJson (x: 'a list) =
            Json.Lens.set id_ (Array (List.map toJson x))

        (* Maps *)

        static member inline ToJson (x: Map<string,'a>) =
            Json.Lens.set id_ (Object (Map.map (fun _ a -> toJson a) x))

        (* Options *)

        static member inline ToJson (x: 'a option) =
            Json.Lens.set id_ ((function | Some a -> toJson a 
                                         | _ -> Null ()) x)

        (* Sets *)

        static member inline ToJson (x: Set<'a>) =
            Json.Lens.set id_ (Array ((Set.toList >> List.map toJson) x))

        (* Tuples *)

        static member inline ToJson ((a, b)) =
            Json.Lens.set id_ (Array [ toJson a; toJson b ])

        static member inline ToJson ((a, b, c)) =
            Json.Lens.set id_ (Array [ toJson a; toJson b; toJson c ])

    (* Functions

        *)

    [<RequireQualifiedAccess>]
    module Json =

        (* Read/Write *)

        let inline readWith fromJson key =
                fromJson >> Json.ofResult
            =<< Json.Prism.get (Json.Object_ >??> Map.key_ key)

        let inline read key =
            readWith fromJson key

        let inline readWithOrDefault fromJson key def =
                function | Some json -> Json.ofResult (fromJson json)
                         | _ -> Json.init def
            =<< Json.Prism.tryGet (Json.Object_ >??> Map.key_ key)

        let inline readOrDefault key def =
            readWithOrDefault fromJson key def

        let inline tryReadWith fromJson key =
                function | Some json -> Some <!> Json.ofResult (fromJson json)
                         | _ -> Json.init None
            =<< Json.Prism.tryGet (Json.Object_ >??> Map.key_ key)

        let inline tryRead key =
            tryReadWith fromJson key

        let inline writeWith toJson key value =
            Json.Prism.set (Json.Object_ >?-> Map.value_ key >??> Option.value_) (toJson value)

        let inline write key value =
            writeWith toJson key value

        let inline writeWithUnlessDefault toJson key def value =
            match value with
            | v when v = def -> Json.ofResult <| Value ()
            | _ -> writeWith toJson key value

        let inline writeUnlessDefault key def value =
            writeWithUnlessDefault toJson key def value

        let inline writeNone key =
            Json.Prism.set (Json.Object_ >?-> Map.value_ key >??> Option.value_) (Json.Null ())

        (* Serialization/Deserialization *)

        let inline deserialize json =
            fromJson json
            |> function | Value a -> a
                        | Error e -> failwith e

        let inline tryDeserialize json =
            fromJson json
            |> function | Value a -> Choice1Of2 a
                        | Error e -> Choice2Of2 e

        let inline serialize a =
            toJson a

(* Patterns

   Active patterns for working with Json data structures, making it
   easier to write code for matching against unions, etc. *)

[<AutoOpen>]
module Patterns =

    /// Parse a Property from a Json Object token, and try to deserialize it to the
    /// inferred type.
    let inline (|Property|_|) key =
            Prism.get (Json.Object_ >??> Map.key_ key)
         >> Option.bind (Json.tryDeserialize >> function | Choice1Of2 json -> Some json
                                                         | _ -> None)