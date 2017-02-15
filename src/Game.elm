module Game exposing (Model, Cell(..), Content(..), initialModel, modelGenerator)

import Grid exposing (..)
import Random as Rnd exposing (Generator)
import Random.Extra exposing (constant)
import Random.List exposing (choose)


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


modelGenerator : Int -> Int -> Int -> Generator Model
modelGenerator rows cols n =
    gridGenerator rows cols n
        |> Rnd.map (\grid -> { grid = grid })


gridGenerator : Int -> Int -> Int -> Generator (Grid Cell)
gridGenerator rows cols n =
    let
        empty =
            initWith rows cols (Hidden Empty)
    in
        randomCoords rows cols n
            |> Rnd.map (List.foldl (\coord -> setCell coord (Hidden Mine)) empty)


randomCoords : Int -> Int -> Int -> Generator (List Coord)
randomCoords rows cols n =
    pickN n (coords rows cols)


coords : Int -> Int -> List Coord
coords rows cols =
    List.range 0 (rows - 1)
        |> List.concatMap
            (\row ->
                List.range 0 (cols - 1)
                    |> List.map (\col -> { row = row, col = col })
            )


pickN : Int -> List a -> Generator (List a)
pickN n xs =
    case n of
        0 ->
            constant []

        n ->
            choose xs
                |> Rnd.andThen
                    (\( xopt, xs2 ) ->
                        case xopt of
                            Nothing ->
                                constant []

                            Just x ->
                                pickN (n - 1) xs2
                                    |> Rnd.map (\xs3 -> x :: xs3)
                    )
