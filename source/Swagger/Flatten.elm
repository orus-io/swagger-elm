module Swagger.Flatten exposing (flatten)

import Swagger.Definition as Definition
    exposing
        ( Definition
        , Definitions
        , definition
        , getName
        , getType
        , singleton
        )
import Swagger.Swagger exposing (Swagger)
import Swagger.Type
    exposing
        ( Items(..)
        , Properties(..)
        , Property(..)
        , Type(..)
        , getPropertyName
        , getPropertyType
        )


flatten : Swagger -> Swagger
flatten ({ definitions } as swagger) =
    { swagger
        | definitions = flattenDefinitions definitions
    }


flattenDefinitions : Definitions -> Definitions
flattenDefinitions =
    Definition.foldl flattenEachRoot <| singleton


flattenEachRoot : Definition -> Definitions -> Definitions
flattenEachRoot definition definitions =
    let
        name =
            getName definition

        newDefinitions =
            case getType definition of
                Object_ props ->
                    flattenProperties [ name ] props definitions

                Array_ items ->
                    flattenItems [ name ] items definitions

                Dict_ type_ ->
                    flattenType [ name ] "Property" type_ definitions

                Enum_ _ _ ->
                    definitions

                String_ _ ->
                    definitions

                Int_ _ ->
                    definitions

                Float_ _ ->
                    definitions

                Bool_ _ ->
                    definitions

                Ref_ _ ->
                    definitions
    in
    Definition.prepend definition newDefinitions


flattenProperties : List String -> Properties -> Definitions -> Definitions
flattenProperties parentNames (Properties props) definitions =
    List.foldl (flattenProperty parentNames) definitions props


flattenProperty : List String -> Property -> Definitions -> Definitions
flattenProperty parentNames prop definitions =
    flattenType parentNames (getPropertyName prop) (getPropertyType prop) definitions


flattenItems : List String -> Items -> Definitions -> Definitions
flattenItems parentNames (Items type_) definitions =
    flattenType parentNames "Item" type_ definitions


flattenType : List String -> String -> Type -> Definitions -> Definitions
flattenType parentNames name type_ definitions =
    let
        childParentNames =
            name :: parentNames

        prependSelf =
            Definition.prepend
                (definition (Just parentNames) name type_)
    in
    case type_ of
        Object_ props ->
            flattenProperties childParentNames props definitions
                |> prependSelf

        Array_ items ->
            flattenItems childParentNames items definitions
                |> prependSelf

        Dict_ typename ->
            flattenType childParentNames "Property" typename definitions

        Enum_ _ _ ->
            definitions
                |> prependSelf

        String_ _ ->
            definitions

        Int_ _ ->
            definitions

        Float_ _ ->
            definitions

        Bool_ _ ->
            definitions

        Ref_ _ ->
            definitions
