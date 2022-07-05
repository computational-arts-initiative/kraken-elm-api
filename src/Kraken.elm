module Kraken exposing
    ( requestProducts
    , requestProductSet
    , requestTilesetsNames
    , requestTilesetContent
    )


import Http

import Kraken.Product as Product exposing (Product)
import Kraken.Tileset as Tileset exposing (Tileset)


url : String
url = "https://kraken.labs.jb.gg/"


brandDataUrl : String
brandDataUrl = "https://resources.jetbrains.com/cai/brand-data/"


requestProducts : Cmd (Result Http.Error (List Product))
requestProducts =
    Http.get
        { url = brandDataUrl ++ "products.json"
        , expect = Http.expectJson identity Product.decodeMany
        }


requestProductSet : Product.Set -> Cmd (Result Http.Error (List Product))
requestProductSet set =
    Http.get
        { url = brandDataUrl ++ "products.json"
        , expect = Http.expectJson identity <| Product.decodeSet set
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
