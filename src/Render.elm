module Render where

import Html (..)
import String

finishCanvas : Html -> Element
finishCanvas canvas =
  node "pre"
    [ "id" := "root" ]
    [ "fontFamily" := "Consolas,Menlo,\"Bitstream Vera Sans Mono\",monospace,\"Powerline Symbols\""
    , "fontSize" := "13px" ]
    ( repeat 24 makeLine )
    |> toElement 100 80

data Glyph = Glyph Color Char

data CanvasLayer = CanvasLayer Int Int [[Glyph]]

-- Takes a canvas layer and turns it into the rendered body of
-- the terminal view to be embedded into the root of the application.
render : CanvasLayer -> Html


