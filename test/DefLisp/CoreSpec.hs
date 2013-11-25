module Deflisp.CoreSpec (main, spec) where

import Test.Hspec

import Deflisp.Core.Types
import Deflisp.Core.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    it "empty list is parsed as an empty list" $ do
      readExpression "()" `shouldBe` LispList []

    it "list with a single number" $ do
      readExpression "(1)" `shouldBe` LispList [LispNumber 1]

    it "list with a number and symbol" $ do
      readExpression "(1 a)" `shouldBe` LispList [LispNumber 1, LispSymbol "a"]

    it "list with a nested list" $ do
      readExpression "(1 (1 2) (3 4))" `shouldBe` LispList [LispNumber 1,
                                                            LispList [LispNumber 1, LispNumber 2],
                                                            LispList [LispNumber 3, LispNumber 4]]
