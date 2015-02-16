module Chiron

open Aether
open FParsec

(* Types

   Simple AST for JSON, with included isomorphisms in Aether format for
   lens/isomorphism based modification of complex JSON structures. *)

type Json =
    | JArray of Json list
    | JBool of bool
    | JNumber of float
    | JNull of unit
    | JObject of Map<string, Json>
    | JString of string

    static member JArrayPIso : PIso<Json, Json list> =
        (function | JArray x -> Some x
                  | _ -> None), JArray

    static member JBoolPIso : PIso<Json, bool> =
        (function | JBool x -> Some x
                  | _ -> None), JBool

    static member JNumberPIso : PIso<Json, float> =
        (function | JNumber x -> Some x
                  | _ -> None), JNumber

    static member JNullPIso : PIso<Json, unit> =
        (function | JNull () -> Some ()
                  | _ -> None), JNull

    static member JObjectPIso : PIso<Json, Map<string, Json>> =
        (function | JObject x -> Some x
                  | _ -> None), JObject

    static member JStringPIso : PIso<Json, string> =
        (function | JString x -> Some x
                  | _ -> None), JString

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

    *)

[<AutoOpen>]
module Lens =

    (* Functions

       Computation expression (monadic) functions for working with the Json
       structure maintained as monadic state. *)

    [<RequireQualifiedAccess>]
    module Json =

        let getLens l : Json<_> =
            fun json ->
                Value (Lens.get l json), json

        let getLensPartial l : Json<_> =
            fun json ->
                match Lens.getPartial l json with
                | Some x -> Value x, json
                | _ -> Error "", json

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

(* Parsing

    *)

[<AutoOpen>]
module Parsing =

    (* Conversion

       Functions for mapping escaped characters and strings to valid
       values. *)

    let private hexToInt x =
        (int x &&& 15) + (int x >>> 6) * 9

    let private hexString h3 h2 h1 h0 =
          (hexToInt h3) * 4096
        + (hexToInt h2) * 256
        + (hexToInt h1) * 16
        + (hexToInt h0)
        |> char
        |> string

    let private mapEsc =
        function | 'c' -> "\b"
                 | 'f' -> "\u000C"
                 | 'n' -> "\n"
                 | 'r' -> "\r"
                 | 't' -> "\t"
                 | c -> string c

    let private plainEscP = 
        anyOf "\"\\/bfnrt" |>> mapEsc

    let private unicodeEscP =
        pchar 'u' >>. pipe4 hex hex hex hex hexString

    let private escP =
        plainEscP <|> unicodeEscP

    let private stringP =
        manySatisfy (fun c -> c <> '"' && c <> '\\')

    let private quotedP =
        between (skipChar '"') (skipChar '"')

    let private literalP =
        quotedP (stringsSepBy stringP ((skipChar '\\') >>. escP))

    (* Patterns

       Functions for parsing items within patterns, such as JSON lists
       and JSON pairs. *)

    let private listItemP item =
        item .>> spaces

    let private listP o c item = 
        between o c (spaces >>. sepBy (listItemP item) (skipChar ',' .>> spaces))

    let private pairP item = 
        tuple2 literalP (spaces >>. pchar ':' >>. spaces >>. item)

    (* Parsers

       Functions for parsing JSON values to Json typed objects, given valid
       JSON data. *)

    let private jsonP, jsonRefP = 
        createParserForwardedToRef ()

    let private jArrayP =
        listP (skipChar '[') (skipChar ']') jsonP |>> JArray

    let private jBoolP = 
        (stringReturn "true" (JBool true)) <|> (stringReturn "false" (JBool false))

    let private jNullP = 
        stringReturn "null" (JNull ())

    let private jNumberP = 
        pfloat |>> JNumber 

    let private jObjectP = 
        listP (skipChar '{') (skipChar '}') (pairP jsonP) |>> (Map.ofList >> JObject)

    let private jStringP = 
        literalP |>> JString

    do jsonRefP := 
        choice [
            jObjectP
            jArrayP
            jStringP
            jBoolP
            jNumberP
            jNullP ]

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

    let format _ =
        ()



(* Mapping

    *)

[<AutoOpen>]
module Mapping =

    open Operators

    (* From

        *)

    type FromJsonDefaults = FromJsonDefaults with

        static member inline FromJson (_: string) =
            Json.getLensPartial (idLens <-?> Json.JStringPIso)

        static member inline FromJson (_: float) =
            Json.getLensPartial (idLens <-?> Json.JNumberPIso)

    let inline internal fromJsonDefaults (_: ^a, b: ^b) =
        ((^a or ^b) : (static member FromJson: ^b -> ^b Json) b)

    let inline internal fromJson json =
        fst (fromJsonDefaults (FromJsonDefaults, Unchecked.defaultof<'a>) json)

    let inline internal readJson json =
        fromJson json
        |> function | Value a -> Json.init a
                    | Error e -> Json.error e
    (* To

        *)

    type ToJsonDefaults = ToJsonDefaults with

        static member inline ToJson (x: string) =
            Json.setLensPartial (idLens <-?> Json.JStringPIso) x

        static member inline ToJson (x: float) =
            Json.setLensPartial (idLens <-?> Json.JNumberPIso) x

    let inline internal toJsonDefaults (_: ^a, b: ^b) =
        ((^a or ^b) : (static member ToJson: ^b -> unit Json) b)

    let inline toJson (x: 'a) =
        snd (toJsonDefaults (ToJsonDefaults, x) (JObject (Map.empty)))

    (* Functions

        *)

    [<RequireQualifiedAccess>]
    module Json =
        
        let inline internal lens key =
                 idLens 
            <-?> Json.JObjectPIso 
            >??> mapPLens key

        let inline read key =
                Json.getLensPartial (lens key) 
            >=> readJson

        let inline tryRead key =
                Json.tryGetLensPartial (lens key)
            >=> function | Some json -> Some <!> readJson json
                         | _ -> Json.init None

        let inline write key value =
            Json.setLensPartial (lens key) (toJson value)

        let inline deserialize json =
            fromJson json
            |> function | Value a -> a
                        | Error e -> failwith e

        let inline tryDeserialize json =
            fromJson json
            |> function | Value a -> Some a
                        | _ -> None