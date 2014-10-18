module Terrain where
import Render (Glyph, CanvasLayer)
import Render
import Matrix
import Matrix (Matrix)

data TerrainKind = Empty
                 | Wall

type TerrainMap = Matrix TerrainKind

glyphOf : TerrainKind -> Glyph
glyphOf tk = 
  case tk of
    Empty -> Render.Empty
    Wall  -> Render.Glyph white black '#'

-- Does nothing currently
render : TerrainMap -> CanvasLayer -> CanvasLayer
render tm canv =
  Render.mergeCanvas (Matrix.map glyphOf tm) canv
