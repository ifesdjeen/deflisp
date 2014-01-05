module Deflisp.CoreSpec (main, spec) where

import Test.Hspec

import Deflisp.Core
import Deflisp.Core.Show
import Deflisp.Core.Types
import Deflisp.Core.Parser

-- import Control.Monad.State

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

  describe "syntax" $ do
    it "empty list evaluates to empty list" $ do
      evalString "()" `shouldBe` (LispList [])

    it "if / else with truthy value evaluates only truthy expression" $ do
      evalString "(if true 1 2)" `shouldBe` (LispNumber 1)

    it "if / else with falsy value evaluates only truthy expression" $ do
      evalString "(if false 1 2)" `shouldBe` (LispNumber 2)

    describe "def" $ do
      it "defines a var" $ do
        evalStrings ["(def a 1)", "a"] `shouldBe` (LispNumber 1)

      it "defines a function" $ do
        evalStrings ["(def myinc (fn [a] (+ 1 a)))",
                     "(myinc 1)"] `shouldBe` (LispNumber 2)

      it "defines a macro" $ do
        evalStrings ["(defmacro or [cond & conds] (list 'if  cond cond  (if (= conds ()) 'false (cons 'or conds))))",
                     "(or false 2 3)"] `shouldBe` (LispNumber 2)

      describe "when macro" $ do
        it "should return a result of expression when trutly value is given" $ do
          evalStrings ["(defmacro when [test & body] (list 'if test (cons 'do body) nil))",
                       "(when true 1)"] `shouldBe` (LispNumber 1)

        it "should return nil when faulty value is given" $ do
          evalStrings ["(defmacro when [test & body] (list 'if test (cons 'do body) nil))",
                       "(when false 1)"] `shouldBe` LispNil


      -- it "apply" $ do
      --   evalStrings ["(defmacro apply [var cond] (cons var cond))",
      --                "(apply + '(1 2 3))"] `shouldBe` (LispList
      --                                                  [LispNumber 1,
      --                                                   ])
