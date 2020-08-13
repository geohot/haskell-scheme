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
    test ["(cond (1 1) (else 2))"] "Number 1"
    test ["(if 1 1 2)"] "Number 1"
    test ["(= 1 1.0)"] "Bool True"

    -- parsing has some problems with spaces
    test ["( * 2 2 )"] "Number 4" -- currently a parse error

    -- the stuff below might technically be more like 1.1.8 stuff,
    -- so not strictly within chapter 1.1.7

    -- this is why "body" is a list
    test ["(define (forthpower n) (define m (* n n)) (* m m))",
          "(forthpower 3)"] "Number 81" -- currently produces "Number 9"

    -- parameter definitions should not leak
    test ["(define (square x) (* x x))",
          "(define x 42)",
          "(square 5)",
          "x"] "Number 42" -- currently produces "Number 5"
    test ["(define (square x) (* x x))",
          "(define (fithpower x) (* (square (square x)) x))",
          "(fithpower 4)"] "Number 1024" -- currently produces "Number 4096"

    -- and in general, local definitions should only be visible inside of the function
    test ["(define m 42)",
          "(define (forthpower n) (define m (* n n)) (* m m))",
          "(forthpower 3)",
          "m"] "Number 42" -- currently produces "Number 9"
