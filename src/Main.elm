module Main exposing (main)

import Html exposing (Html, Attribute, table, tbody, tr, td, text)
import Html.Attributes exposing (style)
import Matrix exposing (Matrix)
import Matrix.Extra
import Array
import Random
import AnimationFrame


main : Program Never Grid Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Grid =
    Matrix Bool


type Msg
    = SetGrid Grid
    | Step


gridGenerator : Random.Generator Grid
gridGenerator =
    Random.list 50 (Random.list 50 Random.bool)
        |> Random.map (Matrix.fromList >> Maybe.withDefault (Matrix.repeat 50 50 False))


init : ( Grid, Cmd Msg )
init =
    ( Matrix.repeat 50 50 False, Random.generate SetGrid gridGenerator )


update : Msg -> Grid -> ( Grid, Cmd Msg )
update msg grid =
    case msg of
        SetGrid grid_ ->
            ( grid_, Cmd.none )

        Step ->
            let
                step x y isAlive =
                    let
                        neighborCount =
                            Matrix.Extra.neighbours x y grid
                                |> List.filter identity
                                |> List.length
                    in
                        if (not isAlive) && neighborCount == 3 then
                            True
                        else if isAlive && (neighborCount == 2 || neighborCount == 3) then
                            True
                        else
                            False
            in
                ( Matrix.indexedMap step grid, Cmd.none )


subscriptions : Grid -> Sub Msg
subscriptions _ =
    AnimationFrame.diffs (always Step)


view : Grid -> Html Msg
view grid =
    table []
        [ tbody [] (List.map row (matrixToList grid))
        ]


matrixToList : Matrix a -> List (List a)
matrixToList matrix =
    List.range 0 (Matrix.height matrix)
        |> List.map (flip Matrix.getRow matrix >> Maybe.withDefault Array.empty >> Array.toList)


row : List Bool -> Html Msg
row =
    List.map cell >> (tr [])


cell : Bool -> Html Msg
cell isAlive =
    td [ cellStyle isAlive ] []


cellStyle : Bool -> Attribute Msg
cellStyle isAlive =
    let
        backgroundColor =
            if isAlive then
                "black"
            else
                "white"
    in
        style
            [ ( "background-color", backgroundColor )
            , ( "width", "5px" )
            , ( "height", "5px" )
            ]
