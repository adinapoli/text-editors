{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Editor.Test  where

import Control.Monad.Identity

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Types
import Text.Editor.Reference.Pure

testEditorTests :: TestTree
testEditorTests = testGroup "Pure Reference tests"
    [ testCase "basic insert rune" basicInsertRuneTest
    , testCase "basic insert line" basicInsertLineTest
    , testCase "basic delete rune" basicDeleteRuneTest
    , testCase "basic delete line" basicDeleteLineTest
    , testCase "basic itemAt rune exists" basicItemAtRuneExistsTest
    , testCase "basic itemAt rune doesn't exists" basicItemAtRuneDoesntExistsTest
    , testCase "basic itemsAt line exists" basicItemsAtLineExistsTest
    , testCase "basic itemsAt line doesn't exists" basicItemsAtLineDoesntExistsTest
    ]


checkApi :: (Monad m, Show a, Eq a)
         => TextEditor backend str m
         -> (forall x. m x -> IO x)
         -> (TextEditor backend str m -> m a)
         -> a
         -> [TextEditor backend str m -> m (TextEditor backend str m)]
         -> Assertion
checkApi editor nt fromEditor expected ops = do
  actual <- nt (fromEditor =<< foldM (\acc op -> op acc) editor ops)
  let msg =  "checkApi failed: expected = " <> show expected 
          <> ", actual = " <> show actual
  assertEqual "checkApi failed" expected actual

checkPureStorage :: String 
                 -> [TextEditor (Reference String) String Identity -> Identity (TextEditor (Reference String) String Identity)]
                 -> Assertion
checkPureStorage expected = 
  checkApi pureStrEditor (pure . runIdentity) (pure . debugDumpStorage) expected

checkPure :: (Show a, Eq a)
          => a
          -> (TextEditor (Reference String) String Identity -> Identity a)
          -> [TextEditor (Reference String) String Identity -> Identity (TextEditor (Reference String) String Identity)]
          -> Assertion
checkPure expected fromEditor = 
  checkApi pureStrEditor (pure . runIdentity) fromEditor expected

basicInsertRuneTest :: Assertion
basicInsertRuneTest = do
  checkPureStorage "hello" [ insert (Pos 0) 'h'
                           , insert (Pos 2) 'l'
                           , insert (Pos 4) 'o'
                           , insert (Pos 1) 'e'
                           , insert (Pos 3) 'l'
                           ]

basicInsertLineTest :: Assertion
basicInsertLineTest =
  checkPureStorage "hello basic world" [ 
      insertLine (Pos 0)  "hello "
    , insertLine (Pos 6)  "basic "
    , insertLine (Pos 12) "world"
    ]

basicDeleteRuneTest :: Assertion
basicDeleteRuneTest =
  checkPureStorage "hello" [ 
      insertLine (Pos 0)  "hello "
    , delete (Pos 5)
    ]

basicDeleteLineTest :: Assertion
basicDeleteLineTest =
  checkPureStorage "hello world" [ 
      insertLine  (Pos 0)  "hello basic world"
    , deleteRange (Range (Pos 6) (Pos 11))
    ]

basicItemAtRuneExistsTest :: Assertion
basicItemAtRuneExistsTest =
  checkPure (Just 'l') (itemAt (Pos 3)) [ insertLine (Pos 0) "hello" ]

basicItemAtRuneDoesntExistsTest :: Assertion
basicItemAtRuneDoesntExistsTest =
  checkPure Nothing (itemAt (Pos 10)) [ insertLine (Pos 0) "hello" ]

basicItemsAtLineExistsTest :: Assertion
basicItemsAtLineExistsTest =
  checkPure "llo" (itemsAt (Range (Pos 2) (Pos 4))) [ 
    insertLine (Pos 0) "hello" 
    ]

basicItemsAtLineDoesntExistsTest :: Assertion
basicItemsAtLineDoesntExistsTest =
  checkPure "" (itemsAt (Range (Pos 0) (Pos 10))) [ 
    insertLine (Pos 0) "hello"
  ]
