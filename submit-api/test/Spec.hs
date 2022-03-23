module Main(main) where

import Test.HUnit
import Models.PGenerator
import Tests
import Eval

main :: IO ()
main = do
  let pool = eval runFailureDepositePoolAndPoolValueAreDifferent
  print pool