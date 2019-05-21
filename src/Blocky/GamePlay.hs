module Blocky.GamePlay
  ( initState
  , select
  , turn
  )
where

import Blocky.BlockTree
import Blocky.Types

initState :: State
initState = State tree player p1 p2
  where
    tree = Block (NilBlock Red) (NilBlock Green) (NilBlock Blue) (NilBlock Yellow)
    player = P1
    p1 = Cursor TL CursorEndpoint
    p2 = Cursor TR CursorEndpoint

updateSplit :: State -> Int -> State
updateSplit (State tree player p1 p2) rnd =
  State (splitAtCursor tree colours cursor) player p1 p2
  where
    cursor | player == P1 = p1 | player == P2 = p2
    colours
      | rnd `mod` 4 == 0 = (Red, Blue, Green, Yellow)
      | rnd `mod` 4 == 1 = (Yellow, Red, Blue, Green)
      | rnd `mod` 4 == 2 = (Green, Yellow, Red, Blue)
      | otherwise        = (Blue, Green, Yellow, Red)

updateRotateRight :: State -> State
updateRotateRight (State tree player p1 p2) =
  State (rotateRightAtCursor tree cursor) player p1 p2
  where
    cursor | player == P1 = p1 | player == P2 = p2

updateRotateLeft :: State -> State
updateRotateLeft (State tree player p1 p2) =
  State (rotateLeftAtCursor tree cursor) player p1 p2
  where
    cursor | player == P1 = p1 | player == P2 = p2

select :: State -> Cursor -> State
select (State tree player p1 p2) selection
  | player == P1 = State tree player selection p2
  | player == P2 = State tree player p1 selection

execCommand :: Int -> Command -> State -> State
execCommand rnd command s =
  case command of
    Split -> updateSplit s rnd
    RotateRight -> updateRotateRight s
    RotateLeft -> updateRotateLeft s

flipPlayer :: State -> State
flipPlayer (State tree P1 p1 p2) = State tree P2 p1 p2
flipPlayer (State tree P2 p1 p2) = State tree P1 p1 p2

turn :: Int -> Command -> State -> State
turn rnd cmd = flipPlayer . execCommand rnd cmd
