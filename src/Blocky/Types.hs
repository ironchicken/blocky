module Blocky.Types
  ( Colour(..)
  , Block(..)
  , CursorPath(..)
  , Cursor(..)
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
