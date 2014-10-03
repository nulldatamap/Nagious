module Render where

import Html
import Html (node, (:=), toElement, Html, text, px)
import String

render : (Int, Int) -> CanvasLayer -> Element
render dims c = renderRoot dims <| renderCanvas c

terminalFont = "Consolas,Menlo,\"Bitstream Vera Sans Mono\",monospace,\"Powerline Symbols\""
terminalFontSize = 13

renderRoot : (Int, Int) -> [ Html ] -> Element
renderRoot (w, h) canvas =
  node "pre"
    [ "id" := "root" ]
    [ "fontFamily" := terminalFont
    , "fontSize" := px terminalFontSize ]
    canvas
    |> toElement w h

data Glyph = Glyph Color Color Char

data CanvasLayer = CanvasLayer [[Glyph]]

-- Takes a canvas layer and turns it into the rendered body of
-- the terminal view to be embedded into the root of the application.
renderCanvas : CanvasLayer -> [ Html ]
renderCanvas (CanvasLayer ls) =
  let renderLine l = node "div" [] [] <| map renderGylph l
      renderGylph (Glyph fc bc chr) = node "span"
                                         []
                                         [ "color" := Html.color fc
                                         , "backgroundColor" := Html.color bc ]
                                         [ text <| String.cons chr "" ]
  in map renderLine ls
