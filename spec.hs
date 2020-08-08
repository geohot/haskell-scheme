module Spec where

import Main hiding (main)
import Test.Hspec
import Data.Foldable

test exprs expected = it (last exprs) $ do
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
    test ["(define (square x) (* x x))", "(square (square 3))"] "Number 81"
    test ["(define (square x) (* x x))",
          "(define (sum-of-squares x y) (+ (square x) (square y)))",
          "(sum-of-squares 3 4)"] "Number 25"
    test ["(if #t 7 3)"] "Number 7"
    test ["(if #f 7 3)"] "Number 3"
    test ["(define (abs x) (if (< x 0) (- x) x))",
          "(+ (abs 5) (abs (- 5)))"] "Number 10"
    test ["(define (abs x) (cond ((> x 0) x) ((= x 0) 0) ((< x 0) (- x))))",
          "(abs (- 1))"] "Number 1"
    test ["(define (abs x) (cond ((< x 0) (- x)) (else x)))",
          "(abs (- 10))"] "Number 10"

