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
           | Empty

type CanvasLayer = [[Glyph]]

mergeCanvas : CanvasLayer -> CanvasLayer -> CanvasLayer
mergeCanvas a b =
  let mergeGylph bottom top =
        case top of
          Empty -> bottom
          _     -> top
  in zipWith (zipWith mergeGylph) a b

-- Takes a canvas layer and turns it into the rendered body of
-- the terminal view to be embedded into the root of the application.
renderCanvas : CanvasLayer -> [ Html ]
renderCanvas ls =
  let renderLine l = node "div" [] [] <| map renderGylph l
      renderGylph gyl =
        case gyl of
          Glyph fc bc chr -> node "span"
                                  []
                                  [ "color" := Html.color fc
                                  , "backgroundColor" := Html.color bc ]
                                  [ text <| String.cons chr "" ]
          -- Empty gylphs are rendered as black spaces
          Empty -> renderGylph <| Glyph black black ' '
  in map renderLine ls
