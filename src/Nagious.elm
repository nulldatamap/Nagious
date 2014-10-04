module Nagious where

import Render
import Window
import Keyboard (KeyCode)
import Keyboard
import Maybe (Maybe(..), maybe)
import Char
import Dict (Dict)
import Dict
import Utils (pickyUnion)

wallGlyph = Render.Glyph white black '#'

gameMap : Render.CanvasLayer
gameMap =
  let makeWalls x y canvas =
        if x == 0 || x == 79
        || y == 0 || y == 23
        then Render.putGlyph wallGlyph x y canvas
        else canvas
  in Render.walkCanvas makeWalls <| Render.newCanvas 80 24

type Player = { x : Int, y : Int }

type GameState = { player : Player, holdTimers : Dict KeyCode Int }

newPlayer = { x = 0, y = 0 }
newState = { player = newPlayer, holdTimers = Dict.empty }

playerGlyph = Render.Glyph white black '@'

renderPlayer : Player -> Render.CanvasLayer -> Render.CanvasLayer
renderPlayer player canvas = Render.putGlyph playerGlyph player.x player.y canvas

movePlayer : Player -> { x : Int, y : Int } -> Player
movePlayer p d = { p | x <- p.x + d.x, y <- p.y + d.y }

handleCommands : [Command] -> GameState -> GameState
handleCommands cmds state =
  let handleCommand cmd st =
    case cmd of
      Move dir -> { st | player <- movePlayer st.player dir }
      _ -> st
  in foldl handleCommand state cmds

-- Filters and handles input
update : [KeyCode] -> GameState -> GameState
update inp state = filterKeys inp state |> uncurry handleCommands

-- Since this uses elm-html which does diffing, this should be cheap to call
-- even though nothing in the scene needs to be re-rendered ( at least I hope so )
render : (Int, Int) -> GameState -> Element
render dim state = Render.render dim <| renderPlayer state.player gameMap

data Command = Move { x : Int, y : Int }

-- How many frames a key should be held before it starts repeating it's signal.
-- This is mostly here to avoid keypresses that were intended to be one move
-- to become multiple.
holdThreshold = 3

filterKeys : [KeyCode] -> GameState -> ([Command], GameState)
filterKeys ks st =
  -- Check if a key has just been pressed or it has exceed the hold threshold
  let holdCheck k =
        let v = Dict.getOrElse 0 k st.holdTimers
        in v == 0 || v > holdThreshold
      -- Updates our hold timers for the keys
      -- Do a picky union of the old timers and the new ones and tick them all once
      updateKeys = Dict.map ((+) 1) <| pickyUnion st.holdTimers
                                    -- Make the list of keys into a map of -1s 
                                    <| Dict.fromList
                                    <| zip ks
                                    <| repeat (length ks) -1
  -- Filter out any un-wanted keys pressed, turn them into commands and update the timers
  in (filter holdCheck ks |> toCommands, { st | holdTimers <- updateKeys })

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
