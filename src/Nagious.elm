module Nagious where

import Render
import Window
import Keyboard (KeyCode)
import Keyboard
import Maybe (Maybe(..), maybe)
import Char
import Dict (Dict)
import Dict
import Utils (pickyUnion, search, unreachable)
import Focus (Focus, create, (=>))
import Focus
import Error (raise)
import Debug (log)

-- ---------------------------------------------------------
-- Welcome to prototype city, refactor inbound any time now!
-- ---------------------------------------------------------

wallGlyph = Render.Glyph white black '#'

rootCanvas : Render.CanvasLayer
rootCanvas =
  let makeWalls x y canvas =
        if x == 0 || x == 79
        || y == 0 || y == 23
        then Render.putGlyph wallGlyph x y canvas
        else canvas
  in Render.walkCanvas makeWalls <| Render.newCanvas 80 24

type GameState = { world : World, holdTimers : Dict KeyCode Int }

               -- Using a records since it's more extensible
data Component = Positionable { x : Int, y : Int }
               | Renderable   { glyph : Render.Glyph }
               | Commandable  { commands : [Command] }

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
positionable : Focus Entity { x : Int, y : Int }
positionable =
  let tryGet ent =
        case getComponent isPositionable ent of
          Positionable p -> p
          _              -> unreachable
      tryMod f r =
        let wrapped v =
              case v of
                Positionable w -> Positionable <| f w
                _              -> unreachable
        in modifyComponent isPositionable wrapped r
  in create tryGet (\f r -> tryMod f r )

commandable : Focus Entity { commands : [Command] }
commandable =
  let tryGet ent =
        case getComponent isCommandable ent of
          Commandable p -> p
          _              -> unreachable
      tryMod f r =
        let wrapped v =
              case v of
                Commandable w -> Commandable <| f w
                _              -> unreachable
        in modifyComponent isCommandable wrapped r
  in create tryGet (\f r -> tryMod f r )

sendCommand : Command -> EntityId -> World -> World
sendCommand cmd eid wrld =
  let sendIt ent = modifyComponent isCommandable
                                   (\x -> case x of
                                            Commandable cm ->
                                              Commandable { cm | commands <- cmd :: cm.commands}
                                            _              -> unreachable)
                                   ent
  in modifyEntity eid sendIt wrld

applyCommand : Entity -> Command -> World -> World
applyCommand ent cmd wrld =
  case cmd of
    Move { x, y } -> Focus.update (entities => (focusEntity ent.id) => positionable)
                                  (\pos -> { pos | x <- pos.x + x, y <- pos.y + y })
                                  wrld
    _             -> wrld

updateCommandable : Component -> Entity -> World -> World
updateCommandable com ent wrld =
  case com of
    Commandable cmds -> let wrld' = foldl (applyCommand ent) wrld cmds.commands
                        in Focus.update (entities => (focusEntity ent.id) => commandable)
                                        (\cmds -> { commands = [] })
                                        wrld'
    _                -> unreachable

type EntityId = Int

type Entity = { components : [Component], id : EntityId }

getComponentMaybe : (Component -> Bool) -> Entity -> Maybe Component
getComponentMaybe pred ent =
  search pred ent.components

getComponent : (Component -> Bool) -> Entity -> Component
getComponent pred ent =
  case getComponentMaybe pred ent of
    Nothing -> raise <| "Tried to access component which isn't present in the given entity: "
                        ++ show ent
    Just r -> r

hasComponent : (Component -> Bool) -> Entity -> Bool
hasComponent pred ent =
  case search pred ent.components of
    Nothing -> False
    _       -> True

modifyComponent : (Component -> Bool) -> (Component -> Component) -> Entity -> Entity
modifyComponent pred func ent =
  { ent | components <- map (\com -> if pred com
                                        then func com
                                        else com)
                            ent.components }

updateComponent : Component -> Entity -> World -> World
updateComponent com ent wrld =
  case com of
    Commandable _ -> updateCommandable com ent wrld
    _             -> wrld

data TerrainKind = Empty
                 | Wall

type TerrainMap = { width : Int, height : Int, cells : [[TerrainKind]] }

type World = { terrain : TerrainMap, entities : Dict Int Entity, playerId : Int }

-- Foci
entities : Focus World (Dict Int Entity)
entities = create .entities (\f r -> { r | entities <- f r.entities })

focusEntity : EntityId -> Focus (Dict Int Entity) Entity
focusEntity eid =
  let maybeWrapper f v =
        case v of
          Nothing -> raise <| "Tried to focus on " ++ show eid ++ " in enties."
          Just w -> Just <| f w
  in create (Dict.getOrFail eid) (\f r -> Dict.update eid (maybeWrapper f) r)

renderWorld : World -> Render.CanvasLayer -> Render.CanvasLayer
renderWorld wrld canvas =
  let renderEntity ent cnv =
        let pos = case getComponent isPositionable ent of
                    Positionable p -> p
                    _              -> unreachable
        in maybe cnv (drawIt cnv pos) <| getComponentMaybe isRenderable ent
      drawIt cnv pos com =
        case com of
             Renderable rnder -> Render.putGlyph rnder.glyph pos.x pos.y cnv
             _ -> unreachable
  in Dict.foldl (\_ -> renderEntity) canvas wrld.entities

updateWorld : World -> World
updateWorld wrld =
  let updateEntity ent innerWrld =
        foldl (\x y -> updateComponent x ent y) innerWrld ent.components
  in Dict.foldl (\_ -> updateEntity) wrld wrld.entities

-- Temporary implementation
getEntity : EntityId -> World -> Entity
getEntity eid wrld =
  case Dict.get eid wrld.entities of
    Nothing -> raise <| "Failed to find an entity with the id: " ++ show eid
    Just en -> en

modifyEntity : EntityId -> (Entity -> Entity) -> World -> World
modifyEntity eid func wrld =
  let failWrapper m =
    case m of
      Just v -> Just <| func v
      Nothing -> raise <| "Tried to access component which isn't present in the given entity: "
                          ++ show eid
  in { wrld | entities <- Dict.update eid failWrapper wrld.entities }

data Command = Move { x : Int, y : Int }

startingPosition = { x = 5, y = 10 }

playerGlyph = Render.Glyph white black '@'

newPlayer : Entity
-- The player id is currently always 0
newPlayer = { components = [ Positionable startingPosition
                           , Renderable   { glyph = playerGlyph }
                           , Commandable  { commands = [] } ]
            , id = 0 }

placeHolderTerrain = { width = 0, height  = 0, cells = [] }

newWorld = { terrain  = placeHolderTerrain
           , entities = Dict.singleton 0 newPlayer
           , playerId = 0 }

newState = { world = newWorld, holdTimers = Dict.empty }

handleCommands : GameState -> [Command] -> GameState
handleCommands state cmds =
  let handleCommand cmd st =
    -- Currently all commands are going straight to the player,
    -- this will not be later on
    { st | world <- sendCommand cmd st.world.playerId st.world }
  in foldl handleCommand state cmds

-- Updates our hold timers for the keys
-- Do a picky union of the old timers and the new ones and tick them all once
updateKeys ks st =
  let ht =Dict.map ((+) 1) <| pickyUnion st.holdTimers
                           -- Make the list of keys into a map of 0s
                           <| Dict.fromList
                           <| zip ks
                           <| repeat (length ks) 0
  in { st | holdTimers <- ht }

-- Filters and handles input
update : [KeyCode] -> GameState -> GameState
update inp state =
  let state' = updateKeys inp state
  in case inp of
      [] -> state'
      _  -> filterKeys inp state
            |> (handleCommands state')
            |> (\st -> { st | world <- updateWorld st.world })

-- Since this uses elm-html which does diffing, this should be cheap to call
-- even though nothing in the scene needs to be re-rendered ( at least I hope so )
render : (Int, Int) -> GameState -> Element
render dim state = Render.render dim <| renderWorld state.world rootCanvas

-- How many frames a key should be held before it starts repeating it's signal.
-- This is mostly here to avoid keypresses that were intended to be one move
-- to become multiple.
holdThreshold = 3

filterKeys : [KeyCode] -> GameState -> [Command]
filterKeys ks st =
  -- Check if a key has just been pressed or it has exceed the hold threshold
  let holdCheck k =
        let v = Dict.getOrElse 0 k st.holdTimers
        in v == 0 || v > holdThreshold
  -- Filter out any unwanted keys pressed, turn them into commands and update the timers
  in filter holdCheck ks |> toCommands

-- Turns a keypress into a game command
toCommands : [KeyCode] -> [Command]
toCommands ks =
  let toCommand k =
        case Char.fromCode k of
          -- The direction keys keycodes turned into characters
          '%'  -> Just <| Move { x = -1, y =  0 } -- left
          '&'  -> Just <| Move { x =  0, y = -1 } -- up
          '\'' -> Just <| Move { x =  1, y =  0 } -- right
          '('  -> Just <| Move { x =  0, y =  1 } -- down
          _    -> Nothing
  in foldr (\x y -> maybe y (\a -> a :: y) x) [] <| map toCommand ks

-- Samples key pressed each frame
-- We keep frames without keypressed in order to reset hold timers,
-- and since we're folding the command lists in the update function
-- we won't do any pointless updates or game ticks.
input : Signal [KeyCode]
input = sampleOn (fps 15) Keyboard.keysDown

main : Signal Element
main = lift2 render Window.dimensions (foldp update newState input)
