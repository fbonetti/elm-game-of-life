import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element, show, flow, right, down, leftAligned)
import Color exposing (blue, white)
import Matrix exposing (Matrix, Location)
import Matrix.Random
import Time exposing (Time)
import Signal
import Random
import Text

-- SIGNALS

main : Signal Element
main =
  Signal.map
    view
    (Signal.foldp update init (Time.fps 60))

-- CONSTANTS

cellSize : number
cellSize = 5

gridWidth : number
gridWidth = 80

gridHeight : number
gridHeight = 80

-- MODEL

type alias Grid = Matrix Bool

init : Grid
init =
  let
    widthGenerator = Random.int gridWidth gridWidth
    heightGenerator = Random.int gridHeight gridHeight
    gridGenerator = Matrix.Random.matrix widthGenerator heightGenerator Random.bool
  in
    Random.generate gridGenerator (Random.initialSeed externalSeed) |> fst

port externalSeed : Int

-- UPDATE

-- Any live cell with fewer than two live neighbours dies, as if caused by under-population.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by over-population.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

neighbours : Grid -> Location -> List Bool
neighbours grid (x,y) =
  List.concatMap (\x -> List.map (\y -> (x,y)) [-1..1]) [-1..1]
    |> List.filter (\(dx,dy) -> not (dx == 0 && dy == 0))
    |> List.map (\(dx,dy) -> ((x - dx) % gridWidth, (y - dy) % gridHeight))
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
update time grid =
  Matrix.mapWithLocation (transformCell grid) grid


-- VIEW

renderCell : (Location, Bool) -> Form
renderCell (location, alive) =
  let
    x = (fst >> toFloat) location
    y = (snd >> toFloat) location
  in
    square cellSize
      |> filled (if alive then blue else white)
      |> move (x * cellSize - 200, y * cellSize - 200)

renderGrid : Grid -> Element
renderGrid grid =
  Matrix.mapWithLocation (,) grid
    |> Matrix.toList
    |> List.concat
    |> List.map renderCell
    |> collage 500 500  

stats : Grid -> Element
stats grid =
  let
    livingCells = (Matrix.flatten >> List.filter identity >> List.length) grid
    deadCells = (Matrix.flatten >> List.filter not >> List.length) grid
  in
    flow down
      [ leftAligned (Text.fromString ("Living cells: " ++ toString livingCells))
      , leftAligned (Text.fromString ("Dead cells: " ++ toString deadCells))
      ]

view : Grid -> Element
view grid =
  flow right
    [ renderGrid grid
    , stats grid
    ]
