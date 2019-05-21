module Main where

import Blocky.GamePlay
import qualified Blocky.Interaction.Console as I
import Blocky.Types
import System.Random (randomIO)

main :: IO ()
main = mainLoop initState

mainLoop :: State -> IO ()
mainLoop st = do
  I.renderGame st
  st' <- I.readSelection st
  cmd <- I.readCommand st
  rnd <- randomIO

  case cmd of
    Quit -> return ()
    _ -> mainLoop (turn rnd cmd st')
