module Blocky.Types
  ( Colour(..)
  , Block(..)
  , CursorPath(..)
  , Cursor(..)
  , Player(..)
  , State(..)
  , Command(..)
  )
where

data Colour
  = Red
  | Blue
  | Yellow
  | Green
  deriving (Eq, Show)

data Block
  = Block Block Block Block Block
  | NilBlock Colour
  deriving (Eq, Show)

data CursorPath
  = TL
  | TR
  | BL
  | BR
  deriving (Eq, Show)

data Cursor
  = Cursor CursorPath Cursor
  | CursorEndpoint
  | CursorFail
  deriving (Eq, Show)

data Player
  = P1
  | P2
  deriving (Eq, Show)

data State
  = State Block Player Cursor Cursor
  deriving (Eq, Show)

data Command
  = Split
  | RotateRight
  | RotateLeft
  | Quit
  deriving (Eq, Show)
