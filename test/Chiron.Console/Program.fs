open Aether
open Aether.Operators
open Chiron

let testPLens =
    idLens <-?> JSON.JObjectPIso >??> mapPLens "hello"

[<EntryPoint>]
let main _ =

    let write =
        json {
            do! modM (setPL testPLens (JString "world"))
            return! getM }

    let jobj = JObject Map.empty
    let result = write jobj

    0
