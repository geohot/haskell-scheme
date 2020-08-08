module Spec where

import Main hiding (main)
import Test.Hspec
import Data.Foldable

test exprs expected = it (head exprs) $ do
    env <- primitiveBindings
    for_ (init exprs) $ evalString env
    result <- evalString env (last exprs)
    shouldBe result expected

main :: IO ()
main = hspec $ do
  describe "tests" $ do
    test ["(+ 137 349)"] "Number 486"
    test ["(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))"] "Number 57"
    test ["(define size 2)", "size"] "Number 2"
    test ["(define (square x) (* x x))", "(square 4)"] "Number 16"

