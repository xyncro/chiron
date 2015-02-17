open Chiron
open Chiron.Operators

type Test =
    { Bool: bool option
      Number: float
      String: string }

    static member FromJson (_: Test) =
            fun b n s ->
                { Bool = b
                  Number = n
                  String = s }
        <!> Json.read "bool"
        <*> Json.read "number"
        <*> Json.read "string"
//
//    static member ToJson (x: Test) =
//            Json.write "bool" x.Bool
//         *> Json.write "number" x.Number
//         *> Json.write "string" x.String
//
let jsonSample =
    """{
      "bool": true,
      "number": 42,
      "string": "hello world"
    }"""

[<EntryPoint>]
let main _ =


    let json = Json.parse jsonSample
    let test = Json.deserialize json
    //let json2 = Json.serialize test

    printfn "%s" test.String

    0
