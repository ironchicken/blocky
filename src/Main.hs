module Main where

import Blocky.GamePlay
import qualified Blocky.Interaction.SDL as I
import Blocky.Types
import System.Random (randomIO)

main :: IO ()
main = mainLoop initState

mainLoop :: State -> IO ()
mainLoop initSt = do
  cxt <- I.initInteraction
  let loop st = do
        I.renderGame cxt st
        st' <- I.readSelection cxt st
        cmd <- I.readCommand cxt st
        rnd <- randomIO

        case cmd of
          Quit -> return ()
          _ -> loop (turn rnd cmd st')

  loop initSt
