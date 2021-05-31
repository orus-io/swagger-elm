module Generate.Encoder exposing (..)

import Codegen.Function exposing (arg, caseof, function, lazy, letin, pipeline)
import Codegen.List exposing (list)
import Codegen.Literal exposing (string)
import Codegen.Tuple exposing (tuple)
import Codegen.Utils exposing (sanitize, uncapitalize)
import Generate.Utils exposing (encoderName, enumValueTypeName, nestedEncoderName, typeName)
import Swagger.Definition as Def exposing (Definition, getFullName, getType)
import Swagger.Type
    exposing
        ( Properties(..)
        , Property(..)
        , Type(..)
        , getItemsType
        , getPropertyName
        , getPropertyType
        )


renderEncoder : Definition -> String
renderEncoder definition =
    let
        name =
            getFullName definition
    in
    function (encoderName <| name)
        [ arg (typeName name) (maybeUnwrapType definition name) ]
        "Json.Encode.Value"
        (renderEncoderBody definition)


maybeUnwrapType : Definition -> String -> String
maybeUnwrapType definition name =
    case getType definition of
        Object_ _ ->
            --"(" ++ typeName name ++ " value" ++ ")"
            "value"

        Array_ _ ->
            "(" ++ typeName name ++ " value" ++ ")"

        Ref_ ref ->
            "(" ++ typeName name ++ " value" ++ ")"

        Dict_ _ ->
            "value"

        Enum_ _ enum ->
            "value"

        String_ _ ->
            "value"

        Int_ _ ->
            "value"

        Float_ _ ->
            "value"

        Bool_ _ ->
            "value"


renderEncoderBody : Definition -> String
renderEncoderBody definition =
    case getType definition of
        Object_ properties ->
            renderObjectBody (getFullName definition) properties

        Array_ items ->
            renderArrayBody (getFullName definition) (getItemsType items)

        Dict_ typeName ->
            renderDictBody (getFullName definition) typeName

        Enum_ _ enum ->
            renderEnumBody (getFullName definition) enum

        String_ _ ->
            renderPrimitiveBody "string"

        Int_ _ ->
            renderPrimitiveBody "int"

        Float_ _ ->
            renderPrimitiveBody "float"

        Bool_ _ ->
            renderPrimitiveBody "bool"

        Ref_ ref ->
            encoderName ref


renderPrimitiveBody : String -> String
renderPrimitiveBody typeName =
    "Json.Encode." ++ typeName ++ " value"


renderArrayBody : String -> Type -> String
renderArrayBody name type_ =
    "Json.Encode.list "
        ++ renderPropertyEncoder name "Item" type_
        ++ " value"


renderDictBody : String -> Type -> String
renderDictBody name typeName =
    "dictEncoder "
        ++ renderPropertyEncoder name "Property" typeName
        ++ " value"


renderObjectBody : String -> Properties -> String
renderObjectBody name (Properties properties) =
    properties
        |> List.map (renderObjectProperty name)
        |> list
        |> (++) "Json.Encode.object <| List.filterMap identity "


renderObjectProperty : String -> Property -> String
renderObjectProperty parentName property =
    let
        propertyEncoder =
            renderPropertyEncoder parentName (getPropertyName property) (getPropertyType property)
    in
    case property of
        Required name type_ ->
            "Just " ++ tuple (string name) (propertyEncoder ++ " value." ++ (uncapitalize <| sanitize name))

        Optional name type_ ->
            "Maybe.map (\\v -> " ++ tuple (string name) (propertyEncoder ++ " v") ++ ") value." ++ (uncapitalize <| sanitize name)

        Default name type_ _ ->
            "Just " ++ tuple (string name) (propertyEncoder ++ " value." ++ (uncapitalize <| sanitize name))


renderPropertyEncoder : String -> String -> Type -> String
renderPropertyEncoder parentName name type_ =
    case type_ of
        Object_ _ ->
            nestedEncoderName parentName name

        Array_ _ ->
            nestedEncoderName parentName name

        Dict_ _ ->
            nestedEncoderName parentName name

        Enum_ _ _ ->
            nestedEncoderName parentName name

        String_ _ ->
            "Json.Encode.string"

        Int_ _ ->
            "Json.Encode.int"

        Float_ _ ->
            "Json.Encode.float"

        Bool_ _ ->
            "Json.Encode.bool"

        Ref_ ref ->
            encoderName ref


renderEnumBody : String -> List String -> String
renderEnumBody parentName enum =
    caseof "value" <|
        List.map (renderEnumEach parentName) enum


renderEnumEach : String -> String -> ( String, String )
renderEnumEach name value =
    ( enumValueTypeName name value, "Json.Encode.string " ++ string value )
