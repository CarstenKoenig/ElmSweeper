module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, button, text)
import Html.Attributes as Attr
import Grid exposing (..)


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }


type alias Model =
    { grid : Grid ()
    }


initialModel : Model
initialModel =
    { grid = initWith 10 20 () }


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


viewCell : a -> Html msg
viewCell cell =
    button [ Attr.style [ ( "width", "20px" ), ( "height", "20px" ) ] ] []
