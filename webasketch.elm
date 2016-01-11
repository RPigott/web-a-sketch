import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import List exposing (..)
import Maybe exposing (withDefault)
import Text exposing (fromString)
import Signal
import Keyboard
--import Mouse
import Window

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update initialBoard input)

marker = { defaultLine | cap <- Round, join <- Smooth, width <- 5 }
tick = fps 30

type alias SketchBoard =
  { size : (Float, Float)
  , locations : List (Float, Float)
  , style : LineStyle
  }
initialBoard = {locations = [(0.0,0.0)], size = (400.0,400.0), style = marker}

type Input = Move {x : Int, y : Int} | Undo | Nothing
input : Signal Input
input =
  Signal.sampleOn tick <|
  Signal.merge 
    (Signal.map Move (Signal.merge Keyboard.arrows Keyboard.wasd))
    (Signal.map (\s -> if s then Undo else Nothing) Keyboard.space)

accumulateInBound : (Float, Float) -> Input -> (Float, Float) -> (Float, Float)  
accumulateInBound (w,h) (Move {x,y}) (px,py) =
  let
    [(low, loh), _, (hiw, hih), _] = rect w h
  in
    (clamp low hiw (px + toFloat (5*x)),clamp loh hih (py + toFloat (5*y)))

update : Input -> SketchBoard -> SketchBoard
update input ({size, locations} as sketchboard) =
  case input of
    Move {x,y} ->
      let
        position = (withDefault (0.0,0.0) (head locations))
        next = accumulateInBound size input position
      in
        if (next == position) then
          sketchboard
        else
          {sketchboard | locations <- next :: locations}
    Undo ->
      {sketchboard | locations <- withDefault [(0.0,0.0)] (tail  locations)}
    Nothing ->
      sketchboard

view : (Int, Int) -> SketchBoard -> Element
view (w,h) {size, locations, style} =
  let 
    (bw, bh) = size
    (x, y) = withDefault (0.0,0.0) (head locations)
    knob = ngon 6 40 |> outlined defaultLine
  in
    collage w h 
      [ rect bw bh
      |> outlined defaultLine
      , path locations
      |> traced style
      , circle 5
      |> filled red
      |> move (x, y)
      , group [knob, fromString "H" |> centered |> toForm]
      |> move ((-bw/2) + 30.0, (-bh/2) - 50.0)
      |> rotate -((x / bw) * 2 * pi)
      , group [knob, fromString "V" |> centered |> toForm]
      |> move (( bw/2) - 30.0, (-bh/2) - 50.0)
      |> rotate -((y / bh) * 2 * pi)
      ]
