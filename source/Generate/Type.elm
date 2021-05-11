module Generate.Type exposing (..)

import Codegen.Type exposing (dict, list, maybe, record, recordField, typeAlias, unionType)
import Codegen.Utils exposing (sanitize)
import Generate.Utils exposing (nestedTypeName, typeName)
import Swagger.Definition as Def exposing (Definition, getFullName, getType)
import Swagger.Type
    exposing
        ( Properties(..)
        , Property(..)
        , Type(..)
        , getItemsType
        )


renderType : Definition -> String
renderType definition =
    let
        name =
            typeName <| getFullName definition

        type_ =
            getType definition

        typeAliasDecl =
            typeAlias name

        unionTypeDecl =
            unionType name

        objectDecl =
            typeAlias <| name ++ "Record"

        arrayDecl =
            \body ->
                "type " ++ name ++ " = " ++ name ++ " (" ++ body ++ ")\n\n"

        recordDecl =
            "type " ++ name ++ " = " ++ name ++ " " ++ name ++ "Record \n\n"
    in
    case type_ of
        String_ _ ->
            typeAliasDecl "String"

        Int_ _ ->
            typeAliasDecl "Int"

        Float_ _ ->
            typeAliasDecl "Float"

        Bool_ _ ->
            typeAliasDecl "Bool"

        Enum_ _ enum ->
            unionTypeDecl <| renderEnum name enum

        Object_ props ->
            (objectDecl <| renderRecord name props)
                ++ recordDecl

        Array_ items ->
            arrayDecl <| list <| renderPropertyType name "Item" <| getItemsType items

        Dict_ typename ->
            typeAliasDecl <| dict <| renderPropertyType name "Property" typename

        Ref_ ref ->
            typeAliasDecl <| typeName ref


renderEnum : String -> List String -> List String
renderEnum name =
    List.map typeName


renderRecord : String -> Properties -> String
renderRecord parentName (Properties properties) =
    record <| List.map (renderProperty parentName) properties


renderProperty : String -> Property -> String
renderProperty parentName prop =
    case prop of
        Required name type_ ->
            recordField (sanitize name) <| renderPropertyType parentName name type_

        Optional name type_ ->
            recordField (sanitize name) <| maybe <| renderPropertyType parentName name type_

        Default name type_ _ ->
            recordField (sanitize name) <| renderPropertyType parentName name type_


renderPropertyType : String -> String -> Type -> String
renderPropertyType parentName name type_ =
    case type_ of
        Object_ _ ->
            nestedTypeName parentName name

        Array_ _ ->
            nestedTypeName parentName name

        Dict_ _ ->
            nestedTypeName parentName name

        Enum_ _ _ ->
            nestedTypeName parentName name

        String_ _ ->
            "String"

        Int_ _ ->
            "Int"

        Float_ _ ->
            "Float"

        Bool_ _ ->
            "Bool"

        Ref_ ref ->
            typeName ref
