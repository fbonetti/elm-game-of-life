import Graphics.Collage exposing (..)
import Time exposing (Time)
import Color exposing (blue)

main =
  Signal.map view
    <| Signal.foldp update 0 (Time.fps 60)

update : Time -> Int -> Int
update _ rotation =
  rotation - 2

view rotation =
  collage 500 500
    [ square 50
        |> filled blue
        |> rotate ((toFloat >> degrees) rotation)
    ]
    