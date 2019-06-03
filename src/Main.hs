module Main where

import Blocky.GamePlay
import qualified Blocky.Interaction.Console as I
import Blocky.Types
import System.Random (randomIO)

main :: IO ()
main = mainLoop initState

mainLoop :: State -> IO ()
mainLoop st = do
  cxt <- I.initInteraction
  I.renderGame cxt st
  st' <- I.readSelection cxt st
  cmd <- I.readCommand cxt st
  rnd <- randomIO

  case cmd of
    Quit -> return ()
    _ -> mainLoop (turn rnd cmd st')
