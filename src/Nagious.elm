module Nagious where

import Render

a = Render.Glyph red 'a'
b = Render.Glyph blue 'b'

main : Element
main =  Render.render 80 80
     <| Render.CanvasLayer [ [ a, b, b, a ]
                           , [ b, a, a, b ]
                           , [ b, a, b, a ] ]

