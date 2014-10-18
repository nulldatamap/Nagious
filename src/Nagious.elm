module Nagious where

import Render (Glyph(..), newCanvas, CanvasLayer)
import Render
import World (World(..), addEntity, canvas)
import World
import Component (..)
import Entity (..)
import Command (..)
import Terrain
import Matrix

import Debug (log)
import Window
import Keyboard (KeyCode)
import Keyboard
import Maybe (Maybe(..), maybe)
import Char
import Dict (Dict)
import Dict
import Utils (pickyUnion)
import Focus

-- Temporary contructors

startingPosition = { x = 5, y = 10 }

playerGlyph = Glyph white black '@'

newPlayer : WorldI -> WorldI
-- The player id is currently always 0
newPlayer wrld =
  let coms = [ Positionable startingPosition
             , Renderable   { glyph = playerGlyph }
             , Commandable  { commands = [] } ]
  in (\(_, x) -> x) <| addEntity wrld coms

placeHolderTerrain =
  let createWalls x y trn = 
        if x == 0 || x == 79
        || y == 0 || y == 23
        then Matrix.set Terrain.Wall x y trn
        else trn
  in Matrix.walk createWalls
                 <| Matrix.repeat 80 24 Terrain.Empty

newWorld : WorldI
newWorld =
  newPlayer <| World { impl = componentImpl
                     , terrain  = placeHolderTerrain
                     , entities = Dict.empty
                     , nextId = 0
                     , canvas = newCanvas 80 24
                     , playerId = 0 }

wallGlyph = Glyph white black '#'

-- More permanent code

type GameState = { world : WorldI, holdTimers : Dict KeyCode Int }

newState : GameState
newState = { world = newWorld, holdTimers = Dict.empty }

-- The first frame is here to render the game before we get any input
-- we make it actually render by giving it a bogus keypress in order
-- to bypass the lazy rendering.
firstFrame : GameState
firstFrame = (update [0] newState)

handleCommands : GameState -> [Command] -> GameState
handleCommands state cmds =
  let handleCommand cmd st =
    -- Currently all commands are going straight to the player,
    -- this will not be later on
    { st | world <- sendCommand cmd 0 st.world }
  in foldl handleCommand state cmds

-- Updates our hold timers for the keys
-- Do a picky union of the old timers and the new ones and tick them all once
updateKeys ks st =
  let ht = Dict.map ((+) 1) <| pickyUnion st.holdTimers
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
            |> (\st -> { st | world <- World.update st.world })

-- Since this uses elm-html which does diffing, this should be cheap to call
-- even though nothing in the scene needs to be re-rendered ( at least I hope so )
render : (Int, Int) -> GameState -> Element
render dim state = Render.render dim (World.val state.world |> .canvas)

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

-- main : Signal Element
main = lift2 render Window.dimensions (foldp update firstFrame input)
