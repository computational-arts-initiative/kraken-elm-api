module Kraken.Product exposing
    ( Product, AtPort, Set, Assets
    , none
    , paletteOf, codeOf, nameOf, idOf, tagline
    , decodeMany, decodeSet
    , hasIcon, iconName, assets
    , ico16, icon, iconFull, square, logoVert, textAsset, textByJBAsset
    , key, shortKey, twoLetter
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


type alias AssetPair =
    { vector : String
    , raster : String
    }


type alias Assets =
    { ico16 : Maybe AssetPair
    , icon : Maybe AssetPair
    , iconFull : Maybe AssetPair
    , logoVert : Maybe AssetPair
    , square : Maybe AssetPair
    , text : Maybe AssetPair
    , textByJB : Maybe AssetPair
    }


type alias ProductRec =
    { id : String
    , code : String
    , name : String
    , palette : Maybe Palette
    , logo : Maybe String
    , key : Maybe String
    , shortKey : Maybe String
    , twoLetter : Maybe String
    , tagline : Maybe String
    }


type Product =
    Product ProductRec Assets


none : Product
none =
    Product
        { id = "", code = "", name = ""
        , palette = Nothing
        , logo = Nothing
        , key = Nothing, shortKey = Nothing, twoLetter = Nothing
        , tagline = Nothing
        } noAssets


paletteOf : Product -> Maybe Palette
paletteOf (Product { palette } _) = palette


codeOf : Product -> String
codeOf (Product { code } _) = code


nameOf : Product -> String
nameOf (Product { name } _) = name


idOf : Product -> String
idOf (Product { id } _) = id


key : Product -> String
key (Product prec _) = prec.key |> Maybe.withDefault "-"


shortKey : Product -> Maybe String
shortKey (Product prec _) = prec.shortKey


twoLetter : Product -> Maybe String
twoLetter (Product prec _) = prec.twoLetter


tagline : Product -> Maybe String
tagline (Product prec _) = prec.tagline


type alias ById = Dict.Dict String Product


toDict : List Product -> ById
toDict =
    Dict.fromList << List.map (\product -> ( idOf product, product ))


decode_ : D.Decoder Product
decode_
    =
        D.map2
            Product
            (
                D.map2
                    identity
                    (D.map8
                        ProductRec
                        (D.field "id" D.string)
                        (D.field "code" D.string)
                        (D.field "name" D.string)
                        (D.map
                            (Maybe.andThen (\paletteList -> if List.isEmpty paletteList then Nothing else Just paletteList))
                            <| D.maybe
                            <| D.field "palette" Palette.decode)
                        (D.maybe <| D.field "logo" D.string)
                        (D.maybe <| D.field "key" D.string)
                        (D.maybe <| D.field "shortKey" D.string)
                        (D.maybe <| D.field "twoLetter" D.string)
                    )
                    (D.maybe <| D.field "tagline" D.string)
            )
            (D.map (Maybe.withDefault noAssets)
                <| D.maybe <| D.field "assets" <| decodeAssets
            )


decodeMany : D.Decoder (List Product)
decodeMany =
    D.map Dict.values <| decodeDict


decodeDict : D.Decoder ById
decodeDict =
    D.field "all"
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


decodeAssetPair : D.Decoder AssetPair
decodeAssetPair =
    D.map2
        AssetPair
        (D.field "vector" D.string)
        (D.field "raster" D.string)


decodeAssets : D.Decoder Assets
decodeAssets =
    D.map7
        Assets
        (D.maybe <| D.field "ico16"    decodeAssetPair)
        (D.maybe <| D.field "icon"     decodeAssetPair)
        (D.maybe <| D.field "iconFull" decodeAssetPair)
        (D.maybe <| D.field "logoVert" decodeAssetPair)
        (D.maybe <| D.field "square"   decodeAssetPair)
        (D.maybe <| D.field "text"     decodeAssetPair)
        (D.maybe <| D.field "textByJB" decodeAssetPair)


hasIcon : Product -> Bool
hasIcon (Product { logo } _) =
    case logo of
        Just _ -> True
        Nothing -> False


iconName : Product -> Maybe String
iconName (Product { logo } _) = logo


byName : Product -> Product -> Order
byName prodA prodB = compare (nameOf prodA) (nameOf prodB)


assets : Product -> Assets
assets (Product _ passets) = passets


ico16 : Product -> Maybe AssetPair
ico16 = assets >> .ico16


icon : Product -> Maybe AssetPair
icon = assets >> .icon


iconFull : Product -> Maybe AssetPair
iconFull = assets >> .iconFull


logoVert : Product -> Maybe AssetPair
logoVert = assets >> .logoVert


square : Product -> Maybe AssetPair
square = assets >> .square


textAsset : Product -> Maybe AssetPair
textAsset = assets >> .text


textByJBAsset : Product -> Maybe AssetPair
textByJBAsset = assets >> .textByJB


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


noAssets : Assets
noAssets =
    { ico16 = Nothing
    , icon = Nothing
    , iconFull = Nothing
    , logoVert = Nothing
    , square = Nothing
    , text = Nothing
    , textByJB = Nothing
    }