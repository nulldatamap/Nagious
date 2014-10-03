module Nagious where

import Render
import Window

a = Render.Glyph red  black 'a'
b = Render.Glyph blue black 'b'
e = Render.Empty

canvas = Render.CanvasLayer [ [ a, b, e, a ]
                            , [ b, e, e, b ]
                            , [ b, a, b, a ] ]

main : Signal Element
main = (\d -> Render.render d canvas) <~ Window.dimensions

