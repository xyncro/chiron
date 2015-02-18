open Chiron
open Chiron.Schema

let schema =
    """{
        "title": "Test",
        "$schema": "http://json-schema.org/schema#"
    }"""

[<EntryPoint>]
let main _ =

    let json = Json.parse schema
    let (s: Schema) = Json.deserialize json

    0