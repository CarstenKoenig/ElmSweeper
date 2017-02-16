module Game exposing (Model, Cell(..), Content(..), initialModel, reveal, cycle)

import Grid exposing (..)
import List.Extra exposing (lift2)
import Set exposing (Set)


type alias Model =
    { grid : Grid Cell
    , gameOver : Bool
    , gameWon : Bool
    , nrMines : Int
    , nrHidden : Int
    }


type Cell
    = Hidden Content
    | Flagged Content
    | Marked Content
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
        { grid = grid
        , gameOver = False
        , gameWon = False
        , nrMines = List.length mines
        , nrHidden = rows * cols
        }


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
    let
        dim1neigh =
            List.range -1 1
    in
        lift2
            (\r c ->
                { coord
                    | col = coord.col + c
                    , row = coord.row + r
                }
            )
            dim1neigh
            dim1neigh


reveal : Coord -> Model -> Model
reveal coord model =
    if model.gameOver then
        model
    else
        let
            showLocations =
                coord :: emptyClosure model.grid coord

            gridUpd =
                showLocations
                    |> List.foldl revealPos model.grid

            hitMine =
                case getCell coord model.grid of
                    Just (Hidden Mine) ->
                        True

                    _ ->
                        False

            gameOverUpd =
                model.gameOver || hitMine

            nrHiddenUpd =
                model.nrHidden - List.length showLocations
        in
            { model
                | grid = gridUpd
                , gameOver = gameOverUpd
                , nrHidden = nrHiddenUpd
                , gameWon = not gameOverUpd && nrHiddenUpd <= model.nrMines
            }


cycle : Coord -> Model -> Model
cycle coord model =
    if model.gameOver then
        model
    else
        let
            ( diff, gridUpd ) =
                cyclePos coord model.grid

            nrHiddenUpd =
                model.nrHidden + diff
        in
            { model
                | grid = gridUpd
                , nrHidden = nrHiddenUpd
                , gameWon = nrHiddenUpd <= model.nrMines
            }


revealPos : Coord -> Grid Cell -> Grid Cell
revealPos coord grid =
    case getCell coord grid of
        Just (Hidden (Empty n)) ->
            setCell coord (Free n) grid

        Just (Hidden Mine) ->
            setCell coord HitMine grid

        _ ->
            grid


cyclePos : Coord -> Grid Cell -> ( Int, Grid Cell )
cyclePos coord grid =
    case getCell coord grid of
        Just (Hidden c) ->
            ( -1, setCell coord (Flagged c) grid )

        Just (Flagged c) ->
            ( 1, setCell coord (Marked c) grid )

        Just (Marked c) ->
            ( 0, setCell coord (Hidden c) grid )

        _ ->
            ( 0, grid )


emptyClosure : Grid Cell -> Coord -> List Coord
emptyClosure grid coord =
    close grid Set.empty (List.singleton coord) []


close : Grid Cell -> Set ( Int, Int ) -> List Coord -> List Coord -> List Coord
close grid seen queue accu =
    case queue of
        [] ->
            accu

        coord :: rest ->
            case getCell coord grid of
                Just (Hidden (Empty 0)) ->
                    if Set.member (toTuple coord) seen then
                        close grid seen rest accu
                    else
                        let
                            seenUpd =
                                Set.insert (toTuple coord) seen

                            queueUpd =
                                left coord :: right coord :: up coord :: down coord :: queue

                            accuUpd =
                                accu ++ neighbours coord
                        in
                            close grid seenUpd queueUpd accuUpd

                _ ->
                    close grid seen rest accu


toTuple : Coord -> ( Int, Int )
toTuple coord =
    ( coord.row, coord.col )


left : Coord -> Coord
left coord =
    { coord | col = coord.col - 1 }


right : Coord -> Coord
right coord =
    { coord | col = coord.col + 1 }


up : Coord -> Coord
up coord =
    { coord | row = coord.row - 1 }


down : Coord -> Coord
down coord =
    { coord | row = coord.row + 1 }
