module Render where

import Html
import Html (node, (:=), toElement, Html, text, px)
import String
import Array
import Array (Array, getOrFail, set)
import Utils (modify, arrayZipWith)
import Matrix
import Matrix (Matrix)

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
           | Empty

type CanvasLayer = Matrix Glyph

newCanvas : Int -> Int -> CanvasLayer
newCanvas w h = Matrix.repeat w h Empty

mergeCanvas : CanvasLayer -> CanvasLayer -> CanvasLayer
mergeCanvas a b =
  let mergeGlyph bottom top =
        case top of
          Empty -> bottom
          _     -> top
  in Matrix.merge mergeGlyph a b

-- Takes a canvas layer and turns it into the rendered body of
-- the terminal view to be embedded into the root of the application.
renderCanvas : CanvasLayer -> [ Html ]
renderCanvas canv =
  let renderLine l = node "div" [] [] <| Array.toList <| Array.map renderGlyph l
      renderGlyph gyl =
        case gyl of
          Glyph fc bc chr -> node "span"
                                  []
                                  [ "color" := Html.color fc
                                  , "backgroundColor" := Html.color bc ]
                                  [ text <| String.cons chr "" ]
          -- Empty glyphs are rendered as black spaces
          Empty -> renderGlyph <| Glyph black black ' '
  in map renderLine (Matrix.rows canv)
