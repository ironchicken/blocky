module BlockTreeSpec
  ( spec )
where

import Blocky.BlockTree
import Blocky.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "rotateRightAtCursor" $ do
    it "should follow the given cursor to find the correct block" $ do
      let tree = Block b b b b
          b = Block (NilBlock Red) (NilBlock Blue) (NilBlock Yellow) (NilBlock Green)
          b' = Block (NilBlock Green) (NilBlock Red) (NilBlock Blue) (NilBlock Yellow)
          cursor = Cursor TL CursorEndpoint
          expected = Block b' b b b

          result = rotateRightAtCursor tree cursor

      result `shouldBe` expected

    it "should rotate the current block when the cursor is at the endpoint" $ do
      let tree = Block (NilBlock Red) (NilBlock Blue) (NilBlock Yellow) (NilBlock Green)
          cursor = CursorEndpoint
          expected = Block (NilBlock Green) (NilBlock Red) (NilBlock Blue) (NilBlock Yellow)

          result = rotateRightAtCursor tree cursor

      result `shouldBe` expected

    it "should error if the given cursor is invalid" $ do
      let tree = Block (NilBlock Red) (NilBlock Blue) (NilBlock Yellow) (NilBlock Green)
          cursor = CursorFail

      (return $ rotateRightAtCursor tree cursor)
        `shouldThrow`
        errorCall "Blocky.BlockTree.modifyTreeAtCursor: Invalid cursor"

  describe "splitAtCursor" $ do
    it "should follow the given cursor to find the correct block" $ do
      let tree = Block (NilBlock Red) (NilBlock Blue) (NilBlock Yellow) (NilBlock Green)
          newBlock = Block (NilBlock Blue) (NilBlock Yellow) (NilBlock Green) (NilBlock Red)
          colours = (Blue, Yellow, Green, Red)
          cursor = Cursor TL CursorEndpoint
          expected = Block newBlock (NilBlock Blue) (NilBlock Yellow) (NilBlock Green)

          result = splitAtCursor tree colours cursor

      result `shouldBe` expected

    it "should split the current block when the cursor is at the endpoint" $ do
      let tree = NilBlock Red
          newBlock = Block (NilBlock Blue) (NilBlock Yellow) (NilBlock Green) (NilBlock Red)
          colours = (Blue, Yellow, Green, Red)
          cursor = CursorEndpoint
          expected = newBlock

          result = splitAtCursor tree colours cursor

      result `shouldBe` expected

    it "should have no effect if the block the cursor points to is already split" $ do
      let tree = Block (NilBlock Blue) (NilBlock Yellow) (NilBlock Green) (NilBlock Red)
          colours = (Blue, Yellow, Green, Red)
          cursor = CursorEndpoint
          expected = tree

          result = splitAtCursor tree colours cursor

      result `shouldBe` expected

    it "should error if the given cursor is invalid" $ do
      let tree = Block (NilBlock Red) (NilBlock Blue) (NilBlock Yellow) (NilBlock Green)
          cursor = Cursor TL (Cursor BR CursorEndpoint)

      (return $ cursorDepth tree cursor)
        `shouldThrow`
        errorCall "Blocky.BlockTree.foldlCursor: Invalid cursor"

  describe "cursorDepth" $ do
    it "should return the correct depth for the given cursor and tree" $ do
      let tree = Block b b b b
          b = Block (NilBlock Red) (NilBlock Blue) (NilBlock Yellow) (NilBlock Green)
          cursor = Cursor TL CursorEndpoint
          depth = 1

      (cursorDepth tree cursor) `shouldBe` depth

    it "should error if the given cursor is invalid for the given tree" $ do
      let tree = Block b b b b
          b = Block (NilBlock Red) (NilBlock Blue) (NilBlock Yellow) (NilBlock Green)
          cursor = Cursor TL (Cursor TR CursorEndpoint)

      (return $ cursorDepth tree cursor)
        `shouldThrow`
        errorCall "Blocky.BlockTree.foldlCursor: Invalid cursor"

  describe "cursorFromList" $ do
    it "should build a cursor from the given list of cursor paths" $ do
      let paths = [TL, TR]
          expectedCursor = (Cursor TL (Cursor TR CursorEndpoint))

      (cursorFromList paths) `shouldBe` expectedCursor

    it "should build a null cursor given an empty list of paths" $ do
      (cursorFromList []) `shouldBe` CursorEndpoint

  describe "mapBlockTreeM" $ do
    it "should flatten the given tree into a list of actions on each NilBlock in the tree" $ do
      let tl = Block (NilBlock Blue) (NilBlock Yellow) (NilBlock Green) (NilBlock Red)
          tree = Block tl (NilBlock Yellow) (NilBlock Green) (NilBlock Red)
          action depth colour = return (depth, colour)
          -- use [] as the test Monad
          expectedList = [ return (2, Blue)
                         , return (2, Yellow)
                         , return (2, Green)
                         , return (2, Red)
                         , return (1, Yellow)
                         , return (1, Green)
                         , return (1, Red)
                         ] :: [[(Int, Colour)]]

      mapBlockTreeM tree action `shouldBe` expectedList
