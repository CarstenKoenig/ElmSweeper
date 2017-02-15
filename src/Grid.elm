module Grid exposing (Grid, nrRows, nrCols, initWith, viewGrid)

import Html exposing (Html, div)
import Array as Arr exposing (Array)


type alias Grid a =
    Array (Array a)


nrRows : Grid a -> Int
nrRows grid =
    Arr.length grid


nrCols : Grid a -> Int
nrCols grid =
    case Arr.get 0 grid of
        Nothing ->
            0

        Just row ->
            Arr.length row


initWith : Int -> Int -> a -> Grid a
initWith rows cols content =
    let
        row =
            Arr.repeat cols content
    in
        Arr.repeat rows row


viewGrid : (a -> Html msg) -> Grid a -> Html msg
viewGrid viewCell grid =
    grid
        |> Arr.map (viewRow viewCell)
        |> Arr.toList
        |> div []


viewRow : (a -> Html msg) -> Array a -> Html msg
viewRow viewCell row =
    row
        |> Arr.map viewCell
        |> Arr.toList
        |> div []
