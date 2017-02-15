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
    = Empty Int
    | Mine


initialModel : Int -> Int -> List Coord -> Model
initialModel rows cols mines =
    let
        empty =
            initWith rows cols (Hidden (Empty 0))

        grid =
            mines |> List.foldl insertMine empty
    in
        { grid = grid }


insertMine : Coord -> Grid Cell -> Grid Cell
insertMine coord grid =
    grid
        |> setCell coord (Hidden Mine)
        |> increaseNeighbours coord


increaseNeighbours : Coord -> Grid Cell -> Grid Cell
increaseNeighbours coord grid =
    neighbours coord
        |> List.foldl increaseEmpty grid


increaseEmpty : Coord -> Grid Cell -> Grid Cell
increaseEmpty coord grid =
    case getCell coord grid of
        Just (Hidden (Empty n)) ->
            setCell coord (Hidden (Empty (n + 1))) grid

        Just (Free n) ->
            setCell coord (Free (n + 1)) grid

        _ ->
            grid


neighbours : Coord -> List Coord
neighbours coord =
    List.range -1 1
        |> List.concatMap
            (\r ->
                List.range -1 1
                    |> List.map
                        (\c ->
                            { coord
                                | col = coord.col + c
                                , row = coord.row + r
                            }
                        )
            )
