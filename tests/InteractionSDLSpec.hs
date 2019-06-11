module InteractionSDLSpec
  ( spec )
where

import Blocky.Interaction.SDL
import Blocky.Types
import Linear.Affine
import Linear.V2
import Test.Hspec

spec :: Spec
spec = do
  describe "blockPosition" $ do
    it "should return the correct position for the given path" $ do
      let gameWidth = 256.0 :: Double
          gameHeight = 256.0 :: Double

      blockPosition [TL] `shouldBe` (P (V2 (round $ gameWidth * 0) (round $ gameWidth * 0)))
      blockPosition [TR] `shouldBe` (P (V2 (round $ gameWidth * 0.5) (round $ gameWidth * 0)))
      blockPosition [BL] `shouldBe` (P (V2 (round $ gameWidth * 0) (round $ gameWidth * 0.5)))
      blockPosition [BR] `shouldBe` (P (V2 (round $ gameWidth * 0.5) (round $ gameWidth * 0.5)))

      blockPosition [TL, TL] `shouldBe` (P (V2 (round $ gameWidth * 0) (round $ gameWidth * 0)))
      blockPosition [TL, TR] `shouldBe` (P (V2 (round $ gameWidth * 0.25) (round $ gameWidth * 0)))
      blockPosition [TL, BL] `shouldBe` (P (V2 (round $ gameWidth * 0) (round $ gameWidth * 0.25)))
      blockPosition [TL, BR] `shouldBe` (P (V2 (round $ gameWidth * 0.25) (round $ gameWidth * 0.25)))

      blockPosition [BR, BR] `shouldBe` (P (V2 (round $ gameWidth * 0.75) (round $ gameWidth * 0.75)))
