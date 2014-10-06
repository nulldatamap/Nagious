module Terrain where

data TerrainKind = Empty
                 | Wall

type TerrainMap = { width : Int, height : Int, cells : [[TerrainKind]] }
