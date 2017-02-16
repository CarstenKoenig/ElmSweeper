module RandomGame exposing (modelGenerator)

import Grid exposing (..)
import Game exposing (..)
import Random as Rnd exposing (Generator)
import Random.Extra exposing (constant)
import Random.List exposing (choose)


modelGenerator : Int -> Int -> Int -> Generator Model
modelGenerator rows cols n =
    randomCoords rows cols n
        |> Rnd.map (\mines -> initialModel rows cols mines)


randomCoords : Int -> Int -> Int -> Generator (List Coord)
randomCoords rows cols n =
    pickN n (coords rows cols)


coords : Int -> Int -> List Coord
coords rows cols =
    List.range 0 (rows - 1)
        |> List.concatMap
            (\row ->
                List.range 0 (cols - 1)
                    |> List.map (\col -> ( row, col ))
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
