module Spec where

import Main hiding (main)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "tests" $ do
    it "(+ 137 349)" $ do
      env <- primitiveBindings
      result <- evalString env "(+ 137 349)" 
      shouldBe result "Number 486"
    it "(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))" $ do
      env <- primitiveBindings
      result <- evalString env "(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))" 
      shouldBe result "Number 57"
    it "(define size 2)" $ do
      env <- primitiveBindings
      evalString env "(define size 2)" 
      result <- evalString env "size" 
      shouldBe result "Number 2"

