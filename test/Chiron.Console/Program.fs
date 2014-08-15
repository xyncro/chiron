open Chiron

// Types

type User =
    { Name: string
      Contact: Contact
      Registered: bool }

and Contact =
    { Email: string }

// Serialization 

type User with

    static member toJSON (x: User) =
        json {
            do! "name" <! x.Name
            do! "contact" <! x.Contact
            do! "registered" <! x.Registered }

and Contact with

    static member toJSON (x: Contact) =
        json {
            do! "email" <! x.Email }

// Deserialization

type User with

    static member fromJSON (_: User) =
        json {
            let! name = !! "name"
            let! contact = !! "contact"
            let! registered = !! "registered"

            return {
                Name = name
                Contact = contact
                Registered = registered } }

and Contact with

    static member fromJSON (_: Contact) =
        json {
            let! email = !! "email"

            return { 
                Email = email } }

// Instances

let example = 
    { Name = "Andrew Cherry"
      Contact = 
        { Email = "andrew@xyncro.com" }
      Registered = true }

// Helpers

let stringify = 
    toJSON >> writeJSON

let parse =
    readJSON >> function | Choice1Of2 x -> fromJSON x | Choice2Of2 x -> Choice2Of2 x


[<EntryPoint>]
let main _ =

    let json = stringify example
    let user : Choice<User, _> = parse json

    0
