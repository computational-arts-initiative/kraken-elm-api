module Kraken.Product exposing
    ( Product, AtPort, Set
    , none
    , paletteOf, codeOf, nameOf, idOf
    , decodeMany, decodeSet
    , hasIcon, iconName
    , byName, jetbrainsFirst, standartSort
    , equal
    , toPort, toDict
    )


import Color
import Dict
import Json.Decode as D

import Kraken.Palette as Palette exposing (Palette)


type Set
    = Internal
    | Public


type alias ProductRec =
    { id : String
    , code : String
    , name : String
    , palette : Maybe Palette
    , logo : Maybe String
    }


type Product =
    Product ProductRec


none : Product
none = Product { id = "", code = "", name = "", palette = Nothing, logo = Nothing }


paletteOf : Product -> Maybe Palette
paletteOf (Product { palette }) = palette


codeOf : Product -> String
codeOf (Product { code }) = code


nameOf : Product -> String
nameOf (Product { name }) = name


idOf : Product -> String
idOf (Product { id }) = id


type alias ById = Dict.Dict String Product


toDict : List Product -> ById
toDict =
    Dict.fromList << List.map (\product -> ( idOf product, product ))


decode_ : D.Decoder ProductRec
decode_
    = D.map5
        ProductRec
        (D.field "id" D.string)
        (D.field "code" D.string)
        (D.field "name" D.string)
        (D.map
            (Maybe.andThen (\paletteList -> if List.isEmpty paletteList then Nothing else Just paletteList))
            <| D.maybe
            <| D.field "palette" Palette.decode)
        (D.maybe <| D.field "logo" D.string)


decodeMany : D.Decoder (List Product)
decodeMany =
    D.map Dict.values <| decodeDict


decodeDict : D.Decoder ById
decodeDict =
    D.field "all"
        <| D.map (Dict.map
                      (\_ rec ->
                           Product rec
                      )
                 )
        <| D.dict decode_


decodeSet : Set -> D.Decoder (List Product)
decodeSet set =
    D.map2
        (\allProducts ->
            List.filterMap
                (\productFromSet ->
                    Dict.get productFromSet allProducts
                )
        )
        decodeDict
        (D.field "_sets"
            <| D.field
                (case set of
                    Internal -> "internal"
                    Public -> "public"
                )
                (D.list D.string)
            )


hasIcon : Product -> Bool
hasIcon (Product { logo }) =
    case logo of
        Just _ -> True
        Nothing -> False


iconName : Product -> Maybe String
iconName (Product { logo }) = logo


byName : Product -> Product -> Order
byName prodA prodB = compare (nameOf prodA) (nameOf prodB)


standartSort : (Product -> Int)
standartSort =
    nameOf >> bySequence
        [ "JetBrains", "Space", "IntelliJ IDEA"
        , "PhpStorm", "PyCharm", "RubyMine"
        , "WebStorm", "CLion", "DataGrip"
        , "DataSpell", "AppCode", "GoLand"
        , "ReSharper", "ReSharper C++", "dotCover"
        , "dotMemory", "dotPeek", "dotTrace"
        , "Rider", "TeamCity", "YouTrack"
        , "Upsource", "Hub", "Kotlin"
        , "Mono", "MPS", "IntelliJ IDEA Edu"
        , "PyCharm Edu", "Datalore", "Qodana"
        ]


bySequence : List String -> (String -> Int)
bySequence sequence =
     let
         dict =
             sequence
             |> List.indexedMap Tuple.pair
             |> List.map (\(idx, v) -> (v, idx))
             |> Dict.fromList
     in
         \name -> Dict.get name dict |> Maybe.withDefault (List.length sequence)



jetbrainsFirst : Product -> Product -> Order
jetbrainsFirst prodA prodB =
    case nameOf prodA of
        "JetBrains" -> LT
        "jetbrains" -> LT
        "Jetbrains" -> LT
        _ -> byName prodA prodB


equal : Product -> Product -> Bool
equal productA productB =
    idOf productA == idOf productB


type alias AtPort =
    { name : String
    , id : String
    , code : String
    , logo : String
    , palette :
        List
            { red : Float, green : Float, blue : Float, alpha : Float, hex : String, rgba : String }
    }


toPort : Product -> AtPort
toPort product =
    { name = nameOf product
    , id = idOf product
    , code = codeOf product
    , logo = iconName product |> Maybe.withDefault ""
    , palette =
         paletteOf product
            |> Maybe.withDefault []
            |> List.map
                (\(color, maybeHex) ->
                    let
                        rgba = Color.toRgba color
                    in
                        { red = rgba.red
                        , green = rgba.green
                        , blue = rgba.blue
                        , alpha = rgba.alpha
                        , hex = maybeHex
                                  |> Maybe.map Palette.hexToString
                                  |> Maybe.withDefault ""
                        , rgba = Color.toCssString color
                        }
               )
    }
