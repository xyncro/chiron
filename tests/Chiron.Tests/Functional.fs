module Chiron.Tests.Functional

open Chiron
open NUnit.Framework
open Swensen.Unquote

let private json =
    JNull ()

[<Test>]
let ``Json.init returns correct values`` () =
    Json.init "test" json =? (Value "test",  json)

[<Test>]
let ``Json.error returns correct values`` () =
    Json.error "error" json =? (Error "error", json)