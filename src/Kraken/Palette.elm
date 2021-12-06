module Kraken.Palette exposing
    ( Palette, Hex, decode, hexToString )


import Json.Decode as D

import Color exposing (Color)


type Hex = Hex String


type alias Palette = List ( Color, Maybe Hex )


decodeColor : D.Decoder ( Color, Maybe Hex )
decodeColor
    = D.map2
          Tuple.pair
          (D.map3
              Color.rgb
              (D.field "r" D.int |> D.map (\r -> toFloat r / 255))
              (D.field "g" D.int |> D.map (\g -> toFloat g / 255))
              (D.field "b" D.int |> D.map (\b -> toFloat b / 255))
          )
          (D.field "hex" (D.maybe D.string)
              |> D.maybe
              |> D.map (Maybe.andThen identity)
              |> D.map (Maybe.map Hex)
          )


decode : D.Decoder Palette
decode = D.list decodeColor


hexToString : Hex -> String
hexToString (Hex str) = str
