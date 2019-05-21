module Blocky.BlockTree
  ( cursorDepth
  , cursorFromList
  , rotateLeftAtCursor
  , rotateRightAtCursor
  , splitAtCursor
  )
where

import Blocky.Types

rotateRight :: Block -> Block
rotateRight (Block tl tr bl br) = Block br tl tr bl
rotateRight b = b

rotateRightAtCursor :: Block -> Cursor -> Block
rotateRightAtCursor b c = modifyTreeAtCursor b (Just c) rotateRight

rotateLeft :: Block -> Block
rotateLeft (Block tl tr bl br) = Block tr bl br tl
rotateLeft b = b

rotateLeftAtCursor :: Block -> Cursor -> Block
rotateLeftAtCursor b c = modifyTreeAtCursor b (Just c) rotateLeft

split :: (Colour, Colour, Colour, Colour) -> Block -> Block
split (tl, tr, bl, br) (NilBlock _) =
  Block (NilBlock tl) (NilBlock tr) (NilBlock bl) (NilBlock br)
split _ b = b

splitAtCursor :: Block -> (Colour, Colour, Colour, Colour) -> Cursor -> Block
splitAtCursor b colours c = modifyTreeAtCursor b (Just c) $ split colours

followCursor :: Block -> Cursor -> (Cursor, Block)
followCursor top CursorEndpoint = (CursorEndpoint, top)
followCursor top CursorFail = (CursorFail, top)
followCursor (Block tl _ _ _) (Cursor TL c) = followCursor tl c
followCursor (Block _ tr _ _) (Cursor TR c) = followCursor tr c
followCursor (Block _ _ bl _) (Cursor BL c) = followCursor bl c
followCursor (Block _ _ _ br) (Cursor BR c) = followCursor br c
followCursor b@(NilBlock _) (Cursor _ _) = (CursorFail, b)

foldlCursor :: Block -> Cursor -> a -> (a -> Block -> a) -> a
foldlCursor top cur acc f =
  case followCursor top cur of
    (CursorEndpoint, b) -> f acc b
    (CursorFail, _) -> error "Blocky.BlockTree.foldlCursor: Invalid cursor"
    (c, b) -> foldlCursor b c (f acc b) f

modifyTreeAtCursor :: Block -> Maybe Cursor -> (Block -> Block) -> Block

modifyTreeAtCursor _ (Just CursorFail) _ =
  error "Blocky.BlockTree.modifyTreeAtCursor: Invalid cursor"

modifyTreeAtCursor b@(NilBlock _) (Just CursorEndpoint) mutate = mutate b
modifyTreeAtCursor b@(NilBlock _) Nothing _ = b
modifyTreeAtCursor b@(NilBlock _) (Just _) mutate =
  modifyTreeAtCursor b (Just CursorFail) mutate

modifyTreeAtCursor b@(Block _ _ _ _) (Just CursorEndpoint) mutate =
  Block treeTL treeTR treeBL treeBR
  where
    (Block tl' tr' bl' br') = mutate b
    treeTL = modifyTreeAtCursor tl' Nothing mutate
    treeTR = modifyTreeAtCursor tr' Nothing mutate
    treeBL = modifyTreeAtCursor bl' Nothing mutate
    treeBR = modifyTreeAtCursor br' Nothing mutate

modifyTreeAtCursor (Block tl tr bl br) Nothing mutate =
  Block treeTL treeTR treeBL treeBR
  where
    treeTL = modifyTreeAtCursor tl Nothing mutate
    treeTR = modifyTreeAtCursor tr Nothing mutate
    treeBL = modifyTreeAtCursor bl Nothing mutate
    treeBR = modifyTreeAtCursor br Nothing mutate
modifyTreeAtCursor (Block tl tr bl br) (Just (Cursor TL c)) mutate =
  Block treeTL treeTR treeBL treeBR
  where
    treeTL = modifyTreeAtCursor tl (Just c) mutate
    treeTR = modifyTreeAtCursor tr Nothing mutate
    treeBL = modifyTreeAtCursor bl Nothing mutate
    treeBR = modifyTreeAtCursor br Nothing mutate
modifyTreeAtCursor (Block tl tr bl br) (Just (Cursor TR c)) mutate =
  Block treeTL treeTR treeBL treeBR
  where
    treeTL = modifyTreeAtCursor tl Nothing mutate
    treeTR = modifyTreeAtCursor tr (Just c)  mutate
    treeBL = modifyTreeAtCursor bl Nothing mutate
    treeBR = modifyTreeAtCursor br Nothing mutate
modifyTreeAtCursor (Block tl tr bl br) (Just (Cursor BL c)) mutate =
  Block treeTL treeTR treeBL treeBR
  where
    treeTL = modifyTreeAtCursor tl Nothing mutate
    treeTR = modifyTreeAtCursor tr Nothing mutate
    treeBL = modifyTreeAtCursor bl (Just c) mutate
    treeBR = modifyTreeAtCursor br Nothing mutate
modifyTreeAtCursor (Block tl tr bl br) (Just (Cursor BR c)) mutate =
  Block treeTL treeTR treeBL treeBR
  where
    treeTL = modifyTreeAtCursor tl Nothing mutate
    treeTR = modifyTreeAtCursor tr Nothing mutate
    treeBL = modifyTreeAtCursor bl Nothing mutate
    treeBR = modifyTreeAtCursor br (Just c) mutate

cursorDepth :: Block -> Cursor -> Int
cursorDepth top cur = foldlCursor top cur 0 inc
  where
    inc i _ = i + 1

cursorFromList :: [CursorPath] -> Cursor
cursorFromList paths = foldl append CursorEndpoint paths
  where
    append :: Cursor -> CursorPath -> Cursor
    append (Cursor p CursorEndpoint) q = Cursor p (Cursor q CursorEndpoint)
    append (Cursor _ CursorFail) _ = error "Blocky.BlockTree.cursorFromList: CursorFail"
    append (Cursor p c) q = Cursor p (append c q)
    append CursorEndpoint p = Cursor p CursorEndpoint
    append CursorFail _ = error "Blocky.BlockTree.cursorFromList: CursorFail"
