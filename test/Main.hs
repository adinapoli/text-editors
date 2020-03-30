module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Test

import qualified Text.Editor.Test.Reference.IO    as Reference.IO
import qualified Text.Editor.Test.Reference.Pure  as Reference.Pure
import qualified Text.Editor.Test.PieceTable.Pure as PieceTable.Pure

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ --Reference.Pure.tests
  -- , Reference.IO.tests
  PieceTable.Pure.tests
  ]
