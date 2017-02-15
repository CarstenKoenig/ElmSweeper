module Grid exposing (Grid, Coord, nrRows, nrCols, initWith, viewGrid, getCell, setCell)

import Html exposing (Html, div)
import Html.Attributes as Attr
import Maybe
import Array as Arr exposing (Array)


type alias Grid a =
    Array (Array a)


type alias Coord =
    { row : Int
    , col : Int
    }


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


getCell : Coord -> Grid a -> Maybe a
getCell coord grid =
    Arr.get coord.row grid
        |> Maybe.andThen
            (\row ->
                Arr.get coord.col row
            )


setCell : Coord -> a -> Grid a -> Grid a
setCell coord value grid =
    case Arr.get coord.row grid of
        Nothing ->
            grid

        Just row ->
            let
                newRow =
                    Arr.set coord.col value row
            in
                Arr.set coord.row newRow grid


viewGrid : (Coord -> a -> Html msg) -> Grid a -> Html msg
viewGrid viewCell grid =
    grid
        |> Arr.indexedMap (viewRow viewCell)
        |> Arr.toList
        |> div []


viewRow : (Coord -> a -> Html msg) -> Int -> Array a -> Html msg
viewRow viewCell nrRow row =
    row
        |> Arr.indexedMap (\nrCol cell -> viewCell { row = nrRow, col = nrCol } cell)
        |> Arr.toList
        |> div [ Attr.style [ ( "clear", "left" ) ] ]
