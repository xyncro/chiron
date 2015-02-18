module Chiron.Schema

open Chiron
open Chiron.Operators
open Freya.Types.Uri

[<RequireQualifiedAccess>]
module Json =

    let inline tryReadMap key f =
            function | Some x ->
                        match f x with
                        | Some x -> Json.init (Some x)
                        | _ -> Json.error ""
                     | _ ->
                        Json.init None
        <=< Json.tryRead key

(* Types *)

type Schema =
    { Title: string option
      Schema: AbsoluteUri option }

    static member FromJson (_: Schema) =
            fun t s ->
                { Title = t
                  Schema = s }
        <!> Json.tryRead "title"
        <*> Json.tryReadMap "$schema" AbsoluteUri.TryParse