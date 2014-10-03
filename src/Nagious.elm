module Nagious where

import Render
import Window

w = Render.Glyph white black '#'
e = Render.Empty
a = Render.Glyph blue  black '@'
b = Render.Glyph black  gray '?'


backgroundLayer : Render.CanvasLayer
backgroundLayer =  [ [ w, w, w, w ]
                   , [ w, e, e, w ]
                   , [ w, e, e, w ] ]

foregroundLayer : Render.CanvasLayer
foregroundLayer = [ [ e, e, e, e ]
                  , [ e, e, a, e ]
                  , [ e, b, e, e ] ]

main : Signal Element
main = (\d -> Render.render d
              (Render.mergeCanvas backgroundLayer foregroundLayer))
        <~ Window.dimensions

