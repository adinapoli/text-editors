module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Reference Editor tests" testEditorTests
  ]
