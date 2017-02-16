module Game exposing (Model, Cell(..), Content(..), initialModel, reveal, cycle, nrFlagged, nrMines, gameWon, gameOver)

import Grid exposing (..)
import List.Extra exposing (lift2)
import Set exposing (Set)
import Array as Arr


type alias Model =
    { grid : Grid Cell
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


gameWon : Model -> Bool
gameWon model =
    not (gameOver model) && nrHidden model <= nrMines model


nrMines : Model -> Int
nrMines model =
    let
        isMine cell =
            case cell of
                Hidden Mine ->
                    True

                Flagged Mine ->
                    True

                Marked Mine ->
                    True

                HitMine ->
                    True

                _ ->
                    False
    in
        model.grid
            |> Arr.toList
            |> List.map (Arr.filter isMine >> Arr.length)
            |> List.sum


gameOver : Model -> Bool
gameOver model =
    let
        hitMine cell =
            case cell of
                HitMine ->
                    True

                _ ->
                    False
    in
        model.grid
            |> Arr.toList
            |> List.any (Arr.toList >> List.any hitMine)


nrHidden : Model -> Int
nrHidden model =
    let
        isHidden cell =
            case cell of
                Free _ ->
                    False

                HitMine ->
                    False

                _ ->
                    True
    in
        model.grid
            |> Arr.toList
            |> List.map (Arr.filter isHidden >> Arr.length)
            |> List.sum


nrFlagged : Model -> Int
nrFlagged model =
    let
        isFlagged cell =
            case cell of
                Flagged _ ->
                    True

                _ ->
                    False
    in
        model.grid
            |> Arr.toList
            |> List.map (Arr.filter isFlagged >> Arr.length)
            |> List.sum


initialModel : Int -> Int -> List Coord -> Model
initialModel rows cols mines =
    let
        empty =
            initWith rows cols (Hidden (Empty 0))

        grid =
            mines |> List.foldl insertMine empty
    in
        { grid = grid
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
neighbours ( row, col ) =
    let
        dim1neigh =
            List.range -1 1
    in
        lift2
            (\r c -> ( row + r, col + c ))
            dim1neigh
            dim1neigh


reveal : Coord -> Model -> Model
reveal coord model =
    if gameOver model || gameWon model then
        model
    else
        let
            showLocations =
                coord :: emptyClosure model.grid coord

            gridUpd =
                showLocations
                    |> List.foldl revealPos model.grid
        in
            { model | grid = gridUpd }


cycle : Coord -> Model -> Model
cycle coord model =
    if gameOver model || gameWon model then
        model
    else
        let
            gridUpd =
                cyclePos coord model.grid
        in
            { model | grid = gridUpd }


revealPos : Coord -> Grid Cell -> Grid Cell
revealPos coord grid =
    case getCell coord grid of
        Just (Hidden (Empty n)) ->
            setCell coord (Free n) grid

        Just (Hidden Mine) ->
            setCell coord HitMine grid

        _ ->
            grid


cyclePos : Coord -> Grid Cell -> Grid Cell
cyclePos coord grid =
    case getCell coord grid of
        Just (Hidden c) ->
            setCell coord (Flagged c) grid

        Just (Flagged c) ->
            setCell coord (Marked c) grid

        Just (Marked c) ->
            setCell coord (Hidden c) grid

        _ ->
            grid


emptyClosure : Grid Cell -> Coord -> List Coord
emptyClosure grid coord =
    close grid Set.empty (List.singleton coord) Set.empty
        |> Set.toList


close : Grid Cell -> Set Coord -> List Coord -> Set Coord -> Set Coord
close grid seen queue accu =
    case queue of
        [] ->
            accu

        coord :: rest ->
            case getCell coord grid of
                Just (Hidden (Empty 0)) ->
                    if Set.member coord seen then
                        close grid seen rest accu
                    else
                        let
                            seenUpd =
                                Set.insert coord seen

                            queueUpd =
                                left coord :: right coord :: up coord :: down coord :: queue

                            accuUpd =
                                Set.union (Set.fromList <| neighbours coord) accu
                        in
                            close grid seenUpd queueUpd accuUpd

                _ ->
                    close grid seen rest accu


left : Coord -> Coord
left ( row, col ) =
    ( row, col - 1 )


right : Coord -> Coord
right ( row, col ) =
    ( row, col + 1 )


up : Coord -> Coord
up ( row, col ) =
    ( row - 1, col )


down : Coord -> Coord
down ( row, col ) =
    ( row + 1, col )
