module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, button, text)
import Html.Attributes as Attr
import Html.Events as Events
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
    | Reveal Coord


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Reveal coord ->
            case getCell coord model.grid of
                Just (Hidden Empty) ->
                    { model | grid = setCell coord (Free 0) model.grid }

                Just (Hidden Mine) ->
                    { model | grid = setCell coord HitMine model.grid }

                _ ->
                    model


view : Model -> Html Msg
view model =
    viewGrid viewCell model.grid


viewCell : Coord -> Cell -> Html Msg
viewCell coord cell =
    let
        attributes =
            [ Attr.style
                [ ( "float", "left" )
                , ( "width", "25px" )
                , ( "height", "25px" )
                , ( "text-align", "center" )
                , ( "vertical-align", "middle" )
                , ( "line-height", "25px" )
                ]
            ]
    in
        case cell of
            Hidden _ ->
                button (Events.onClick (Reveal coord) :: attributes) []

            Free 0 ->
                div attributes []

            Free nr ->
                div attributes [ text (toString nr) ]

            HitMine ->
                div attributes [ text "X" ]
