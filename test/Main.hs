module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Test

import qualified Text.Editor.Test.Reference.IO     as Reference.IO
import qualified Text.Editor.Test.Reference.Pure   as Reference.Pure
import qualified Text.Editor.Test.PieceTable.Types as PieceTable.Types
import qualified Text.Editor.Test.PieceTable.Pure  as PieceTable.Pure
import qualified Text.Editor.Test.PieceTable.IO    as PieceTable.IO

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Reference.Pure.tests
  -- , Reference.IO.tests
  , PieceTable.Types.tests
  , PieceTable.Pure.tests
  --, PieceTable.IO.tests
  ]
