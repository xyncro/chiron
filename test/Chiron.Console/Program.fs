open Chiron

type Test =
    { Text: string
      Number: float }

let jsonSample =
    """{
      "text": "hello world",
      "number": 21.5
    }"""

[<EntryPoint>]
let main _ =

    let read =
        json {
            let! text = Json.read "text"
            let! number = Json.read "number"

            return {
                Text = text
                Number = number } }

    let test, _ = read (Json.parse jsonSample)

    0
