module Deflisp.CoreSpec (main, spec) where

import Test.QuickCheck.Monadic
import Test.Hspec as H
import Test.Hspec.Expectations.Lifted as L

import Deflisp.Core
import Deflisp.Core.Show
import Deflisp.Core.Types
import Deflisp.Core.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    it "empty list is parsed as an empty list" $ do
      readExpression "()" `H.shouldBe` LispList []

    it "list with a single number" $ do
      readExpression "(1)" `H.shouldBe` LispList [LispNumber 1]

    it "list with a number and symbol" $ do
      readExpression "(1 a)" `H.shouldBe` LispList [LispNumber 1, LispSymbol "a"]

    it "list with a nested list" $ do
      readExpression "(1 (1 2) (3 4))" `H.shouldBe` LispList [LispNumber 1,
                                                            LispList [LispNumber 1, LispNumber 2],
                                                            LispList [LispNumber 3, LispNumber 4]]

  -- describe "functions" $ do
  --   it "anonymous function evaluates" $ do
  --     env <- freshEnv
  --     run $ evalString env "((fn [a] a) 1)" `L.shouldBe` (LispNumber 1)
