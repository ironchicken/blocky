{-# LANGUAGE OverloadedStrings #-}

module Blocky.Interaction.SDL
  ( initInteraction
  , renderGame
  , readSelection
  , readCommand
  , blockPosition
  )
where

import Blocky.BlockTree
import Blocky.Types
import Data.Word (Word8)
import Foreign.C.Types
import SDL
import System.IO

black, red, blue, yellow, green, greyBorder :: V4 Word8
black = V4 0 0 0 255
red = V4 255 0 0 255
blue = V4 0 0 255 255
yellow = V4 255 255 0 255
green = V4 0 255 0 255
greyBorder = V4 100 100 100 255

gameOffsetX, gameOffsetY :: CInt
gameOffsetX = 100
gameOffsetY = 100

gameWidth, gameHeight :: Double
gameWidth = 256.0
gameHeight = 256.0

borderWidth :: CInt
borderWidth = 3

initialBlockSize :: Double
initialBlockSize = gameWidth

initInteraction :: IO Renderer
initInteraction = do
  initializeAll
  window <- createWindow "Blocky" defaultWindow
  createRenderer window (-1) defaultRenderer

renderGame :: Renderer -> State -> IO ()
renderGame renderer (State tree _ _ _) = do
  rendererDrawColor renderer $= black
  clear renderer
  sequence_ $ mapBlockTreeM tree (renderBlock renderer)
  present renderer

blockColour :: Colour -> V4 Word8
blockColour Red = red
blockColour Blue = blue
blockColour Yellow = yellow
blockColour Green = green

renderBlock :: Renderer -> [CursorPath] -> Colour -> IO ()
renderBlock renderer path colour = do
  rendererDrawColor renderer $= greyBorder
  fillRect renderer blockBorder

  rendererDrawColor renderer $= blockColour colour
  fillRect renderer block

  where
    blockBorder =
      Just (Rectangle
            (blockPosition path)
            (V2 sz sz))
    block =
      Just (Rectangle
            (translateV2 (blockPosition path) (V2 borderWidth borderWidth))
            (V2 (sz - borderWidth * 2) (sz - borderWidth * 2)))
    sz = round $ initialBlockSize * (1 / (2 ** fromIntegral (length path)))

blockPosition :: [CursorPath] -> Point V2 CInt
blockPosition path = P (V2 left top)
  where
    left = gameOffsetX + round (gameWidth * foldr leftAccum 0.0 (zip path [1..]))
    top = gameOffsetY + round (gameHeight * foldr topAccum 0.0 (zip path [1..]))
    leftAccum (TL, _) lt = lt
    leftAccum (TR, d) lt = lt + (1 / (2 ** d))
    leftAccum (BL, _) lt = lt
    leftAccum (BR, d) lt = lt + (1 / (2 ** d))
    topAccum (TL, _) tp = tp
    topAccum (TR, _) tp = tp
    topAccum (BL, d) tp = tp + (1 / (2 ** d))
    topAccum (BR, d) tp = tp + (1 / (2 ** d))

translateV2 :: (Num a) => Point V2 a -> V2 a -> Point V2 a
translateV2 (P (V2 x y)) (V2 dx dy) = P (V2 (x + dx) (y + dy))

readSelection :: Renderer -> State -> IO State
readSelection _ (State tree player p1 p2) = do
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

readCommand :: Renderer -> State -> IO Command
readCommand c st = do
  hSetBuffering stdout NoBuffering
  putStr "Command: "
  input <- readLn
  case input of
    "quit" -> return Quit
    "rr" -> return RotateRight
    "rl" -> return RotateLeft
    "s" -> return Split
    _ -> readCommand c st
