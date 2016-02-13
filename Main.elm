import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element, show)
import Color exposing (blue, white)
import Matrix exposing (Matrix, Location)
import Time exposing (Time)
import Signal
import Debug

-- SIGNALS

main : Signal Element
main =
  Signal.map
    view
    (Signal.foldp update init (Time.every Time.second))

-- MODEL

type alias Grid = Matrix Bool

init : Grid
init =
  Matrix.fromList
    [ [False, False, False, False, False]
    , [False, False, False, False, False]
    , [False, True, True, True, False]
    , [False, False, True, False, False]
    , [False, False, False, False, False]
    ]

-- UPDATE

-- Any live cell with fewer than two live neighbours dies, as if caused by under-population.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by over-population.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

neighbours : Grid -> Location -> List Bool
neighbours grid (x,y) =
  List.concatMap (\x -> List.map (\y -> (x,y)) [-1..1]) [-1..1]
    |> List.filter (\(dx,dy) -> not (dx == 0 && dy == 0))
    |> List.map (\(dx,dy) -> (x - dx, y - dy))
    |> List.filterMap ((flip Matrix.get) grid)

count : (a -> Bool) -> List a -> Int
count predicate =
  (List.filter predicate >> List.length)

numberOfLivingNeighbors : Grid -> Location -> Int
numberOfLivingNeighbors grid location =
  count identity (neighbours grid location)
  

transformCell : Grid -> Location -> Bool -> Bool
transformCell grid location alive =
  let
    livingNeighbors = numberOfLivingNeighbors grid location
  in
    if alive && livingNeighbors < 2 then
      False
    else if alive && (livingNeighbors == 2 || livingNeighbors == 3) then
      True
    else if alive && livingNeighbors > 3 then
      False
    else if (not alive) && livingNeighbors == 3 then
      True
    else
      False

update : Time -> Grid -> Grid
update _ grid =
  Matrix.mapWithLocation (transformCell grid) grid


-- VIEW

renderCell : (Location, Bool) -> Form
renderCell (location, alive) =
  let
    x = (fst >> toFloat) location
    y = (snd >> toFloat) location
  in
    square 5
      |> filled (if alive then blue else white)
      |> move (x * 5,y * 5)

view : Grid -> Element
view grid =
  Matrix.mapWithLocation (,) grid
    |> Matrix.toList
    |> List.concat
    |> List.map renderCell
    |> collage 500 500