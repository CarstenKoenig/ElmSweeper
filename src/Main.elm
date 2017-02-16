module Main exposing (..)

import Html exposing (Html, program, div, button, text)
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Grid exposing (..)
import Game exposing (..)
import RandomGame exposing (modelGenerator)
import Platform.Sub
import Platform.Cmd as Cmd


main : Program Never Model Msg
main =
    program
        { init = ( initialModel 15 25 [], Random.generate InitModel (modelGenerator 15 25 40) )
        , view = view
        , subscriptions = \_ -> Platform.Sub.none
        , update = update
        }


type Msg
    = InitModel Model
    | Reveal Coord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitModel model ->
            model ! []

        Reveal coord ->
            reveal coord model ! []


view : Model -> Html Msg
view model =
    let
        gameStatus =
            if model.gameWon then
                Html.h3 [] [ text "YOU won" ]
            else if model.gameOver then
                Html.h3 [] [ text "you are dead - sorry" ]
            else
                Html.h3 [] [ text "good luck pal" ]
    in
        div []
            [ gameStatus
            , viewGrid viewCell model.grid
            ]


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
