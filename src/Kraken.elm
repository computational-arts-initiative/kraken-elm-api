module Kraken exposing
    ( requestProducts
    , requestTilesetsNames
    , requestTilesetContent
    )


import Http

import Kraken.Product as Product exposing (Product)
import Kraken.Tileset as Tileset exposing (Tileset)


url : String
url = "https://kraken.labs.jb.gg/"


requestProducts : Cmd (Result Http.Error (List Product))
requestProducts =
    Http.get
        { url = url ++ "palettes/get_all"
        , expect = Http.expectJson identity Product.decodeMany
        }


requestTilesetsNames : Cmd (Result Http.Error (List Tileset.Id))
requestTilesetsNames =
    Http.get
        { url = url ++ "tiles/get_tilesets_name"
        , expect = Http.expectJson identity Tileset.decodeIds
        }


requestTilesetContent : Tileset.Subject -> Cmd (Result Http.Error (List (Tileset.Id, List Tileset.TileUrl)))
requestTilesetContent subject =
    Http.get
        { url = url ++ "tiles/get_tilesets?prefix=" ++ subject
        , expect = Http.expectJson identity Tileset.decodeMany
        }
