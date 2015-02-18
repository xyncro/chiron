module Chiron

open Aether
open FParsec

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
                | Some x ->
                    printfn "json: %A" json
                    printfn "x: %A" x
                    Value x, json
                | _ ->
                    printfn "json error: %A" json
                    Error "", json

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

   Functions for parsing string JSON data to Json types, using
   FParsec.

   Functions parse and tryParse are effectively static,
   while import parses the provided string JSON and replaces the
   current state of a Json<'a> function. *)

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
        listP (skipChar '[') (skipChar ']') jsonP |>> Array

    let private jBoolP = 
        (stringReturn "true" (Bool true)) <|> (stringReturn "false" (Bool false))

    let private jNullP = 
        stringReturn "null" (Null ())

    let private jNumberP = 
        pfloat |>> Number 

    let private jObjectP = 
        listP (skipChar '{') (skipChar '}') (pairP jsonP) |>> (Map.ofList >> Object)

    let private jStringP = 
        literalP |>> String

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

    let inline internal get iso =
        Json.getLensPartial (idLens <-?> iso)

    (* Defaults *)

    type FromJsonDefaults = FromJsonDefaults with

        static member inline FromJson (_: bool) =
            get Json.BoolPIso

        static member inline FromJson (_: decimal) =
            decimal <!> get Json.NumberPIso

        static member inline FromJson (_: float) =
            get Json.NumberPIso

        static member inline FromJson (_: int) =
            int <!> get Json.NumberPIso

        static member inline FromJson (_: int16) =
            int16 <!> get Json.NumberPIso

        static member inline FromJson (_: int64) =
            int64 <!> get Json.NumberPIso

        static member inline FromJson (_: single) =
            single <!> get Json.NumberPIso

        static member inline FromJson (_: string) =
            get Json.StringPIso

        static member inline FromJson (_: uint16) =
            uint16 <!> get Json.NumberPIso

        static member inline FromJson (_: uint32) =
            uint32 <!> get Json.NumberPIso

        static member inline FromJson (_: uint64) =
            uint64 <!> get Json.NumberPIso

    (* Mapping Functions
    
       Functions for applying to FromJson function to Json to produce
       new instances of 'a where possible, including folding the FromJson
       function across a list of Json objects. *)

    let inline internal fromJsonDefaults (a: ^a, _: ^b) =
        ((^a or ^b) : (static member FromJson: ^a -> ^a Json) a)

    let inline internal fromJson x : Json<'a> =
        fun json -> fst (fromJsonDefaults (Unchecked.defaultof<'a>, FromJsonDefaults) x), json

    let inline internal foldFromJson e f xs =
        fun json ->
            List.fold (fun (r, _) x ->
                match r with
                | Error e ->
                    Error e, json
                | Value xs ->
                    match fromJson x json with
                    | Value x, _ -> Value (f x xs), json
                    | Error e, _ -> Error e, json) (Value e, json) (List.rev xs)

    (* Defaults *)

    type FromJsonDefaults with

        (* Arrays *)

        static member inline FromJson (_: 'a array) : Json<'a array> =
                Json.getLens idLens
            >=> function | Array x -> foldFromJson [||] (fun x xs -> Array.append [| x |] xs) x
                         | _ -> Json.error "array"

        (* Lists *)

        static member inline FromJson (_: 'a list) : Json<'a list> =
                Json.getLens idLens
            >=> function | Array x -> foldFromJson [] (fun x xs -> x :: xs) x
                         | _ -> Json.error "list"

        (* Maps *)

        static member inline FromJson (_: Map<string,'a>) : Json<Map<string,'a>> =
                Json.getLens idLens
            >=> function | Object x ->
                            let k, v = (Map.toList >> List.unzip) x
                            List.zip k >> Map.ofList <!> foldFromJson [] (fun x xs -> x :: xs) v
                         | _ -> Json.error "map"

        (* Sets *)

        static member inline FromJson (_: Set<'a>) : Json<Set<'a>> =
                Json.getLens idLens
            >=> function | Array x -> foldFromJson Set.empty Set.add x
                         | _ -> Json.error "set"

        (* Options *)

        static member inline FromJson (_: 'a option) : Json<'a option> =
                Json.getLens idLens
            >=> function | Null _ -> Json.init None
                         | x -> Some <!> fromJson x

        (* Tuples *)

        static member inline FromJson (_: 'a * 'b) : Json<'a * 'b> =
                Json.getLens idLens
            >=> function | Array (a :: b :: []) ->
                                fun a b -> a, b
                            <!> fromJson a 
                            <*> fromJson b
                         | _ -> Json.error "tuple2"

        static member inline FromJson (_: 'a * 'b * 'c) : Json<'a * 'b * 'c> =
                Json.getLens idLens
            >=> function | Array (a :: b :: c :: []) ->
                                fun a b c -> a, b, c
                            <!> fromJson a 
                            <*> fromJson b
                            <*> fromJson c
                         | _ -> Json.error "tuple3"

    (* Functions

        *)

    [<RequireQualifiedAccess>]
    module Json =
        
        let inline internal lens key =
                 idLens 
            <-?> Json.ObjectPIso 
            >??> mapPLens key

        (* Read *)

        let inline read key =
                printfn "read: %s" key

                Json.getLensPartial (lens key) 
            >=> fromJson

        let inline tryRead key =
                Json.tryGetLensPartial (lens key)
            >=> function | Some json -> Some <!> fromJson json
                         | _ -> Json.init None

        (* Deserialization *)

        let inline deserialize json =
            fromJson json (Null ())
            |> fst
            |> function | Value a -> a
                        | Error e -> failwith e

        let inline tryDeserialize json =
            fromJson json (Null ())
            |> fst
            |> function | Value a -> Some a
                        | _ -> None






//        let inline serialize a =
//            toJson a
//
//        let inline write key value =
//            Json.setLensPartial (lens key) (toJson value)
//

//    (* To
//
//        *)
//
//    let inline internal set iso v =
//        Json.setLensPartial (idLens <-?> iso) v
//
//    type ToJsonDefaults = ToJsonDefaults with
//
//        static member inline ToJson (x: bool) =
//            set Json.JBoolPIso x
//
//        static member inline ToJson (x: float) =
//            set Json.JNumberPIso x
//
//        static member inline ToJson (x: string) =
//            set Json.JStringPIso x
//
//    let inline internal toJsonDefaults (_: ^a, b: ^b) =
//        ((^a or ^b) : (static member ToJson: ^b -> unit Json) b)
//
//    let inline internal toJson (x: 'a) =
//        snd (toJsonDefaults (ToJsonDefaults, x) (JObject (Map.empty)))
