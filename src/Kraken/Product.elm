module Kraken.Product exposing
    ( Product, AtPort
    , empty
    , paletteOf, codeOf, nameOf
    , decodeMany
    , hasIcon, iconName
    , byName, jetbrainsFirst, standartSort
    , equal
    , toPort
    )


import Color
import Dict
import Json.Decode as D

import Kraken.Palette as Palette exposing (Palette)


type Product = Product { code : String, name : String, palette : Palette }


empty : Product
empty = Product { code = "", name = "", palette = [] }


paletteOf : Product -> Palette
paletteOf (Product { palette }) = palette


codeOf : Product -> String
codeOf (Product { code }) = code


nameOf : Product -> String
nameOf (Product { name }) = name


decode_ : D.Decoder { code : String, palette : Palette }
decode_
    = D.map2
        (\code palette -> { code = code, palette = palette })
        (D.field "code" D.string)
        (D.field "palette" Palette.decode)


decodeMany : D.Decoder (List Product)
decodeMany =
    D.field "products"
        <| D.map Dict.values
        <| D.map (Dict.map
                      (\productName { code, palette } ->
                           Product { name = productName, code = code, palette = palette }
                      )
                 )
        <| D.dict decode_


hasIcon : Product -> Bool
hasIcon product =
    case iconName product of
        Just _ -> True
        Nothing -> False


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


iconName : Product -> Maybe String
iconName (Product { name }) =
    case name of
        "JetBrains" -> Just "logojb"
        "Space" -> Just "Space"
        "IntelliJ IDEA" -> Just "IntelliJ-IDEA"
        "PhpStorm" -> Just "PhpStorm"
        "PyCharm" -> Just "PyCharm"
        "RubyMine" -> Just "RubyMine"
        "WebStorm" -> Just "WebStorm"
        "CLion" -> Just "CLion"
        "DataGrip" -> Just "DataGrip"
        "DataSpell" -> Just "DataSpell"
        "AppCode" -> Just "AppCode"
        "GoLand" -> Just "GoLand"
        "ReSharper" -> Just "ReSharper"
        "ReSharper C++" -> Just "ReSharperCPP"
        "dotCover" -> Just "dotCover"
        "dotMemory" -> Just "dotMemory"
        "dotPeek" -> Just "dotPeek"
        "dotTrace" -> Just "dotTrace"
        "Rider" -> Just "Rider"
        "TeamCity" -> Just "TeamCity"
        "YouTrack" -> Just "YouTrack"
        "Upsource" -> Just "Upsource"
        "Hub" -> Just "Hub"
        "Kotlin" -> Just "Kotlin"
        "Mono" -> Just "Mono"
        "MPS" -> Just "MPS"
        "IntelliJ IDEA Edu" -> Just "IntelliJ-IDEA-Edu"
        "PyCharm Edu" -> Just "PyCharm-Edu"
        "Datalore" -> Just "Datalore"
        _ -> Nothing


equal : Product -> Product -> Bool
equal productA productB =
    nameOf productA == nameOf productB


type alias AtPort =
    { name : String
    , palette :
        List
            { red : Float, green : Float, blue : Float, alpha : Float, hex : String, rgba : String }
    }


toPort : Product -> AtPort
toPort product =
    { name = nameOf product
    , palette =
         paletteOf product
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
