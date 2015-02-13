open Aether
open Aether.Operators
open Chiron

let jsonSample =
    """{
      "firstName": "John",
      "lastName": "Smith",
      "isAlive": true,
      "age": 25,
      "height_cm": 167.6,
      "address": {
        "streetAddress": "21 2nd Street",
        "city": "New York",
        "state": "NY",
        "postalCode": "10021-3100"
      },
      "phoneNumbers": [
        {
          "type": "home",
          "number": "212 555-1234"
        },
        {
          "type": "office",
          "number": "646 555-4567"
        }
      ],
      "children": [],
      "spouse": null
    }"""

let streetPLens =
         idLens
    <-?> Json.JObjectPIso
    >??> mapPLens "address"
    <??> Json.JObjectPIso
    >??> mapPLens "streetAddress"
    <??> Json.JStringPIso

[<EntryPoint>]
let main _ =

    let write =
        json {
            let! street = Json.getPartial streetPLens
            do! Json.setPartial streetPLens "25 New Street"

            return street }

    let data = Json.parse jsonSample
    let result, newData = write data

    0
