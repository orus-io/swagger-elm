module Swagger.Decode exposing (..)

import Json.Decode as Json exposing (Decoder, Value, andThen, bool, decodeValue, field, float, int, keyValuePairs, lazy, list, map, oneOf, string, value)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Regex
import Swagger.Definition exposing (Definition, Definitions, definition, definitions)
import Swagger.Swagger exposing (Swagger)
import Swagger.Type
    exposing
        ( Items(..)
        , Properties(..)
        , Property(..)
        , Ref
        , Type(..)
        , getDefault
        )


type alias Name =
    String


trio : a -> b -> c -> ( a, b, c )
trio a b c =
    ( a, b, c )


decode : a -> Decoder a
decode =
    Json.succeed


regex : String -> Regex.Regex
regex =
    Regex.fromString >> Maybe.withDefault Regex.never


decodeSwagger : Decoder Swagger
decodeSwagger =
    decode Swagger
        |> required "definitions" decodeTypes


decodeTypes : Decoder Definitions
decodeTypes =
    keyValuePairs decodeType
        |> map (List.map (\( name, type_ ) -> definition Nothing name type_))
        |> map definitions


decodeType : Decoder Type
decodeType =
    lazy
        (\_ ->
            decode Tuple.pair
                |> optional "type" string ""
                |> maybe "$ref" string
                |> andThen decodeTypeByType
        )


decodeTypeByType : ( String, Maybe String ) -> Decoder Type
decodeTypeByType ( type_, ref ) =
    case ref of
        Just ref_ ->
            decodeRef

        Nothing ->
            case type_ of
                "string" ->
                    decodeString

                "integer" ->
                    decodePrimitive Int_

                "number" ->
                    decodePrimitive Float_

                "boolean" ->
                    decodePrimitive Bool_

                "array" ->
                    decodeArray

                _ ->
                    decodeObject


decodeRef : Decoder Type
decodeRef =
    decode identity
        |> required "$ref" string
        |> andThen extractRef
        |> map Ref_


extractRef : String -> Decoder Ref
extractRef ref =
    let
        parsed =
            List.head (Regex.findAtMost 1 (regex "^#/definitions/(.+)$") ref)
                |> Maybe.andThen (List.head << .submatches)
    in
    case parsed of
        Just (Just ref_) ->
            Json.succeed ref_

        _ ->
            Json.fail ("Unparseable reference " ++ ref)


decodePrimitive : (Maybe String -> Type) -> Decoder Type
decodePrimitive constructor =
    decode identity
        |> maybe "default" decodeAlwaysString
        |> map constructor


decodeString : Decoder Type
decodeString =
    decode Tuple.pair
        |> maybe "default" decodeAlwaysString
        |> maybe "enum" (list string)
        |> map (apply2 stringOrEnum)


stringOrEnum : Maybe String -> Maybe (List String) -> Type
stringOrEnum default enum =
    case enum of
        Nothing ->
            String_ default

        Just value ->
            Enum_ default value


decodeArray : Decoder Type
decodeArray =
    decode identity
        |> required "items" (lazy (\_ -> decodeType))
        |> map (Array_ << Items)


decodeDict : Decoder Type
decodeDict =
    decode Dict_
        |> required "additionalProperties" (lazy (\_ -> decodeType))


decodeObject : Decoder Type
decodeObject =
    decode trio
        |> optional "required" (list string) []
        |> maybe "properties" (lazy (\_ -> keyValuePairs decodeType))
        |> maybe "additionalProperties" (lazy (\_ -> decodeType))
        |> map objectOrDict



{-
   According to the [Swagger specification]
   (http://swagger.io/specification/#model-with-map-dictionary-properties-88)
   a dict is defined by the following json:
   {
     "type": "object",
     "additionalProperties": {
       "type": "string"
     }
   }
   So if the key "properties" is not set and the key "additionalProperties" is set
   we interpret it as a dictionary instead of an object.
-}


objectOrDict : ( List String, Maybe (List ( String, Type )), Maybe Type ) -> Type
objectOrDict ( required, properties, additionalProperties ) =
    case ( properties, additionalProperties ) of
        ( Nothing, Just addProps ) ->
            Dict_ addProps

        ( Just props, _ ) ->
            Object_ <| Properties <| List.map (property required) props

        _ ->
            Object_ <| Properties []


decodeProperties : ( List String, List ( String, Type ) ) -> List Property
decodeProperties ( required, properties ) =
    List.map (property required) properties


property : List String -> ( String, Type ) -> Property
property required ( name, type_ ) =
    case List.any ((==) name) required of
        True ->
            Required name type_

        False ->
            case getDefault type_ of
                Just default ->
                    Default name type_ default

                Nothing ->
                    Optional name type_



-- helpers


apply2 : (a -> b -> c) -> ( a, b ) -> c
apply2 fn ( a, b ) =
    fn a b


decodeAlwaysString : Decoder String
decodeAlwaysString =
    oneOf
        [ string
        , int |> map String.fromInt
        , float |> map String.fromFloat
        , bool
            |> map
                (\b ->
                    if b then
                        "true"

                    else
                        "false"
                )
        ]


maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe name decoder =
    optional name (map Just decoder) Nothing
