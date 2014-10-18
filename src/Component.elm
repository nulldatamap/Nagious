module Component where

import Render (Glyph)
import Command (..)
import Entity (..)
import World (World, entities, focusEntity, canvas)
import Utils (unreachable)
import Matrix

import Focus
import Focus (Focus, create, (=>))

type WorldI = World Component
type EntityI = Entity Component WorldI

               -- Using a records since it's more extensible
data Component = Positionable { x : Int, y : Int }
               | Renderable   { glyph : Glyph }
               | Commandable  { commands : [Command] }

-- Implement the component interface
componentImpl : ComponentI Component WorldI
componentImpl =
  let upd com ent wrld =
        case com of
          Commandable _ -> updateCommandable com ent wrld
          Renderable _  -> renderRenderable com ent wrld
          _             -> wrld
  in ComponentI upd

-- Predicates
isPositionable : Component -> Bool
isPositionable com =
  case com of
    Positionable _ -> True
    _              -> False

isRenderable : Component -> Bool
isRenderable com =
  case com of
    Renderable _ -> True
    _            -> False

isCommandable : Component -> Bool
isCommandable com =
  case com of
    Commandable _ -> True
    _             -> False

-- Foci

-- I hate that this is so far from DRY
-- But since they're data constructors you can't
-- pass them to the case of clause, which makes
-- this impossible to generalize :(
positionable : Focus EntityI { x : Int, y : Int }
positionable =
  let tryGet ent =
        case getComponent isPositionable ent of
          Positionable p -> p
          _              -> unreachable "positionable"
      tryMod f r =
        let wrapped v =
              case v of
                Positionable w -> Positionable <| f w
                _              -> unreachable "positionable"
        in modifyComponent isPositionable wrapped r
  in create tryGet (\f r -> tryMod f r )

commandable : Focus EntityI { commands : [Command] }
commandable =
  let tryGet ent =
        case getComponent isCommandable ent of
          Commandable p -> p
          _              -> unreachable "commandable"
      tryMod f r =
        let wrapped v =
              case v of
                Commandable w -> Commandable <| f w
                _              -> unreachable "commandable"
        in modifyComponent isCommandable wrapped r
  in create tryGet (\f r -> tryMod f r )

-- TODO: renderable : Focus Entity { glyph : Render.Glyph }
-- - Not needed yet.

-- Renderable functions

renderRenderable : Component -> EntityI -> WorldI -> WorldI
renderRenderable rndr ent wrld = 
  let (Positionable pos) = getComponent isPositionable ent
      (Renderable r) = rndr
  in Focus.update canvas (\canv -> Matrix.set r.glyph pos.x pos.y canv ) wrld

-- Commandable functions

sendCommand : Command -> EntityId -> WorldI -> WorldI
sendCommand cmd eid wrld =
  let sendIt ent = modifyComponent isCommandable
                                   (\x -> case x of
                                            Commandable cm ->
                                              Commandable { cm | commands <- cmd :: cm.commands}
                                            _              -> unreachable "sendCommand")
                                   ent
  in Focus.update (entities => (focusEntity eid)) (\ent -> sendIt ent) wrld

applyCommand : EntityI -> Command -> WorldI -> WorldI
applyCommand ent cmd wrld =
  case cmd of
    Move { x, y } -> Focus.update (entities => (focusEntity ent.id) => positionable)
                                  (\pos -> { pos | x <- pos.x + x, y <- pos.y + y })
                                  wrld
    _             -> wrld

updateCommandable : Component -> EntityI -> WorldI -> WorldI
updateCommandable com ent wrld =
  case com of
    Commandable cmds -> let wrld' = foldl (applyCommand ent) wrld cmds.commands
                        in Focus.update (entities => (focusEntity ent.id) => commandable)
                                        (\cmds -> { commands = [] })
                                        wrld'
    _                -> unreachable "updateCommandable"

