module Kraken.Tileset exposing
    ( Tileset(..)
    , Id, Subject, Name
    , TileUrl
    , decodeIds
    , decodeMany
    , sortOrder
    , matches
    )


import Dict
import Json.Decode as D


type alias TileUrl = String


type alias Subject = String


type alias Name = String


type Tileset = Tileset Id (List TileUrl)


type alias Id = { subject : Subject, name : Name }


decodeId : D.Decoder (Maybe Id)
decodeId =
    D.string
        |> D.map
            (\fullId ->
                case String.split "/" fullId of
                    ("tiles" :: subject :: name :: _) ->
                        Just { subject = subject, name = name }
                    _ -> Nothing
            )


decodeIds : D.Decoder (List Id)
decodeIds = D.map (List.filterMap identity) <| D.list decodeId


emptyId : Id
emptyId = { subject = "?", name = "?" }


stringToId : String -> Maybe Id
stringToId str =
    case String.split "/" str of
        subject::name::_ -> Just { subject = subject, name = name }
        _ -> Nothing


decodeMany : D.Decoder (List (Id, List TileUrl))
decodeMany =
    D.field "tile_sets"
        <| D.map (List.map <| Tuple.mapFirst (Maybe.withDefault emptyId << stringToId))
        <| D.map Dict.toList
        <| D.dict
        <| D.list D.string


matchingTiles : String -> List TileUrl -> List TileUrl
matchingTiles needle =
    List.filter <| String.contains needle


matches : String -> List (Id, List TileUrl) -> List (Id, List TileUrl)
matches needle =
    List.map
        (\( { subject, name }, tiles) ->
             if (String.contains needle subject) || (String.contains needle name) then
                 ( { subject = subject, name = name }, tiles )
             else
             --    Nothing
                 ( { subject = subject, name = name }, tiles |> matchingTiles needle )
             -- Just ( { subject = subject, name = name }, tiles |> matchingTiles needle )
        )


sortOrder : Subject -> Int
sortOrder subject =
    case subject of
        "Logos_Icons" -> 0
        "Tiler" -> 1
        "Kvant" -> 2
        _ -> 3
