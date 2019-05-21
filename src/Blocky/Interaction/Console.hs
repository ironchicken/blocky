module Blocky.Interaction.Console
  ( renderGame
  , readSelection
  , readCommand
  )
where

import Blocky.Types
import Blocky.BlockTree (cursorFromList)
import System.IO

renderGame :: State -> IO ()
renderGame = print . show

readSelection :: State -> IO State
readSelection (State tree player p1 p2) = do
  hSetBuffering stdout NoBuffering
  putStr "Selection: "
  sel <- readLn

  case player of
    P1 -> return (State tree player (cursor sel) p2)
    P2 -> return (State tree player p1 (cursor sel))

  where
    cursor = cursorFromList . map toPath . words

    toPath "tl" = TL
    toPath "tr" = TR
    toPath "bl" = BL
    toPath "br" = BR
    toPath _ = error "Blocky.Interaction.Console.readSelection: Bad path"

readCommand :: State -> IO Command
readCommand st = do
  hSetBuffering stdout NoBuffering
  putStr "Command: "
  input <- readLn
  case input of
    "quit" -> return Quit
    "rr" -> return RotateRight
    "rl" -> return RotateLeft
    "s" -> return Split
    _ -> readCommand st
