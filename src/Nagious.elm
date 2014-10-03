module Nagious where

import Render
import Window

a = Render.Glyph red  black 'a'
b = Render.Glyph blue black 'b'

canvas = Render.CanvasLayer [ [ a, b, b, a ]
                            , [ b, a, a, b ]
                            , [ b, a, b, a ] ]

main : Signal Element
main = (\d -> Render.render d canvas) <~ Window.dimensions

