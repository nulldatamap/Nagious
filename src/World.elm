module World where

import Terrain
import Terrain (TerrainMap)
import Entity (..)
import Render (Glyph(..), CanvasLayer, newCanvas)
import Matrix

import Dict (Dict)
import Dict
import Error (raise)
import Focus
import Focus (Focus, create, get)

type EntityW a = Entity a (World a)

data World a = World { impl : ComponentI a (World a)
                     , terrain : TerrainMap
                     , entities : Dict EntityId (EntityW a)
                     , nextId : EntityId
                     , canvas : CanvasLayer
                     , playerId : EntityId }

val : World a -> { impl : ComponentI a (World a)
                 , terrain : TerrainMap
                 , entities : Dict EntityId (EntityW a)
                 , nextId : EntityId
                 , canvas : CanvasLayer
                 , playerId : EntityId }
val w = 
  let (World v) = w
  in v

addEntity : World a -> [a] -> (EntityW a, World a)
addEntity wrld coms = 
  let w = val wrld
      ent = { impl = w.impl
            , components = coms
            , id = w.nextId }
      wrld' = World { w | nextId <- w.nextId + 1
                        , entities <- Dict.insert w.nextId ent w.entities }
  in (ent, wrld')

getEntities : World a -> Dict EntityId (EntityW a)
getEntities w =
  val w |> .entities

-- Foci
entities : Focus (World a) (Dict Int (EntityW a))
entities =
  let ue f r = 
    let vr = val r
    in World { vr | entities <- f vr.entities }
  in create getEntities ue

canvas : Focus (World a) CanvasLayer
canvas =
  let upd f r =
    let v = val r
    in World { v | canvas <- f v.canvas }
  in create (\r -> val r |> .canvas) upd

terrain : Focus (World a) TerrainMap
terrain =
  let upd f r =
    let v = val r
    in World { v | terrain <- f v.terrain }
  in create (\r -> val r |> .terrain) upd

focusEntity : EntityId -> Focus (Dict Int (EntityW a)) (EntityW a)
focusEntity eid =
  let maybeWrapper f v =
        case v of
          Nothing -> raise <| "Tried to focus on " ++ show eid ++ " in enties."
          Just w -> Just <| f w
  in create (Dict.getOrFail eid) (\f r -> Dict.update eid (maybeWrapper f) r)

-- Game loop functionality
startRender : World a -> World a
startRender wrld =
  Focus.update canvas
               (\canv -> Terrain.render (get terrain wrld) <| newCanvas 80 24 )
               wrld

update : World a -> World a
update wrld =
  let updateEntity ent innerWrld =
        foldl (\x y -> updateComponent x ent y) innerWrld ent.components
  in Dict.foldl (\_ -> updateEntity)
                (startRender wrld)
                <| getEntities wrld
