module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, button, text)
import Html.Attributes as Attr
import Grid exposing (..)


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }


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


initialModel : Model
initialModel =
    { grid = initWith 15 20 (Hidden Empty) }


type Msg
    = NoOp


update : Msg -> a -> a
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    viewGrid viewCell model.grid


viewCell : Cell -> Html msg
viewCell cell =
    let
        attributes =
            [ Attr.style [ ( "width", "25px" ), ( "height", "25px" ) ] ]
    in
        case cell of
            Hidden _ ->
                button attributes []

            Free nr ->
                div attributes [ text (toString nr) ]

            HitMine ->
                div attributes [ text "X" ]
