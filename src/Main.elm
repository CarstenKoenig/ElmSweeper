module Main exposing (..)

import Html exposing (Html, Attribute, program, div, button, text)
import Html.Attributes as Attr
import Html.Events as Events exposing (on, onWithOptions)
import Random
import Grid exposing (..)
import Game exposing (..)
import RandomGame exposing (modelGenerator)
import Platform.Sub
import Platform.Cmd as Cmd
import Json.Decode as Json


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
    | NoOp
    | Reveal Coord
    | Cycle Coord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitModel model ->
            model ! []

        NoOp ->
            model ! []

        Reveal coord ->
            reveal coord model ! []

        Cycle coord ->
            cycle coord model ! []


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
                button
                    (onRightClick (Cycle coord) (Reveal coord)
                        :: onContextMenu (Cycle coord)
                        :: attributes
                    )
                    []

            Flagged _ ->
                button
                    (onRightClick (Cycle coord) NoOp
                        :: onContextMenu (Cycle coord)
                        :: attributes
                    )
                    [ text "!" ]

            Marked _ ->
                button
                    (onRightClick (Cycle coord) NoOp
                        :: onContextMenu (Cycle coord)
                        :: attributes
                    )
                    [ text "?" ]

            Free 0 ->
                div attributes []

            Free nr ->
                div attributes [ text (toString nr) ]

            HitMine ->
                div attributes [ text "X" ]


onRightClick : msg -> msg -> Attribute msg
onRightClick msgRight msgElse =
    let
        which =
            Json.maybe (Json.at [ "which" ] Json.string)
                |> Json.map
                    (\nr ->
                        case nr of
                            Just "3" ->
                                True

                            _ ->
                                False
                    )

        button =
            Json.maybe (Json.at [ "button" ] Json.string)
                |> Json.map
                    (\nr ->
                        case nr of
                            Just "2" ->
                                True

                            _ ->
                                False
                    )

        rightMb =
            Json.map2
                (\a b ->
                    if a || b then
                        msgRight
                    else
                        msgElse
                )
                which
                button
    in
        on "click" rightMb


onContextMenu : msg -> Attribute msg
onContextMenu msg =
    let
        opts =
            Events.defaultOptions
    in
        onWithOptions "contextmenu"
            ({ opts
                | preventDefault = True
                , stopPropagation = True
             }
            )
            (Json.succeed msg)
