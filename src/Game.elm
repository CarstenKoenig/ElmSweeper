module Game exposing (Model, Cell(..), Content(..), initialModel)

import Grid exposing (..)


type alias Model =
    { grid : Grid Cell
    }


type Cell
    = Hidden Content
    | Free Int
    | HitMine


type Content
    = Empty
    | Mine


initialModel : Int -> Int -> Model
initialModel rows cols =
    { grid = initWith rows cols (Hidden Empty) }
