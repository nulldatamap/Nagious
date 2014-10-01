module Nagious where

import Render
import Window

a = Render.Glyph red 'a'
b = Render.Glyph blue 'b'

canvas = Render.CanvasLayer [ [ a, b, b, a ]
                            , [ b, a, a, b ]
                            , [ b, a, b, a ] ]

main : Signal Element
main = (\d -> Render.render d canvas) <~ Window.dimensions

