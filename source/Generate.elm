module Generate exposing (generate, main)

import Generate.Swagger exposing (render)
import Json.Decode as Decode exposing (decodeString)
import Platform exposing (Program)
import Ports
import Swagger.Decode as Swagger exposing (decodeSwagger)
import Swagger.Flatten exposing (flatten)


type alias Flags =
    String


main : Program Flags () Never
main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        }


init : Flags -> ( (), Cmd Never )
init json =
    ( ()
    , case generate json of
        Ok result ->
            Ports.printAndExitSuccess result

        Err err ->
            Ports.printAndExitFailure err
    )


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.mapError Decode.errorToString
        |> Result.map (flatten >> render)
