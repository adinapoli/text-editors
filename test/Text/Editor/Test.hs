{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Editor.Test  where

import Control.Monad.Identity

import Data.Proxy
import Data.String

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Types
import Text.Editor.Editable as Editable

testEditorTests :: forall backend str m. (
                   Editable str
                 , Rune str ~ Char
                 , Monad m
                 , Show str
                 )
                => String
                -- ^ Label for the test group.
                -> Proxy backend
                -> Proxy str
                -> Proxy m
                -> CheckStorage backend str m
                -> CheckResult  backend str m
                -> TestTree
testEditorTests lbl Proxy Proxy Proxy checkStorage checkResult = 
    testGroup lbl [ 
      testCase "basic insert rune" (basicInsertRuneTest checkStorage)
    , testCase "basic insert runes" (basicInsertRunesTest checkStorage)
    , testCase "basic insert line" (basicInsertLineTest checkStorage)
    , testCase "basic delete rune beginning" (basicDeleteRuneBeginningTest checkStorage)
    , testCase "basic delete rune middle" (basicDeleteRuneMiddleTest checkStorage)
    , testCase "basic delete rune end" (basicDeleteRuneEndTest checkStorage)
    , testCase "basic delete line beginning" (basicDeleteLineBeginningTest checkStorage)
    , testCase "basic delete line middle" (basicDeleteLineMiddleTest checkStorage)
    , testCase "basic delete line end" (basicDeleteLineEndTest checkStorage)
    --, testCase "basic itemAt rune exists" (basicItemAtRuneExistsTest checkResult)
    --, testCase "basic itemAt rune doesn't exists" (basicItemAtRuneDoesntExistsTest checkResult)
    --, testCase "basic itemsAt line exists" (basicItemsAtLineExistsTest checkResult)
    --, testCase "basic itemsAt line doesn't exists" (basicItemsAtLineDoesntExistsTest checkResult)
    ]

type CheckStorage backend str m
  =  str 
  -> [TextEditor backend str m -> m (TextEditor backend str m)]
  -> Assertion

type CheckResult backend str m
  = forall a. (Show a, Eq a, Editable str)
  => m a
  -> (TextEditor backend str m -> m a)
  -> [TextEditor backend str m -> m (TextEditor backend str m)]
  -> Assertion

checkApi :: (Monad m, Show a, Eq a)
         => m (TextEditor backend str m)
         -> (forall x. m x -> IO x)
         -> (TextEditor backend str m -> m a)
         -> m a
         -> [TextEditor backend str m -> m (TextEditor backend str m)]
         -> Assertion
checkApi editorM nt fromEditor expectedM ops = do
  editor   <- nt editorM 
  expected <- nt expectedM
  actual <- nt (fromEditor =<< foldM (\acc op -> op acc) editor ops)
  let msg =  "checkApi failed: expected = " <> show expected 
          <> ", actual = " <> show actual
  assertEqual "checkApi failed" expected actual

{------------------------------------------------------------------------------
 The actual tests
------------------------------------------------------------------------------}

basicInsertRuneTest :: (Editable str, Rune str ~ Char)
                    => CheckStorage backend str m 
                    -> Assertion
basicInsertRuneTest checkStorage = do
  checkStorage (fromString "abc") [
      insert (Pos 0) 'a'
    , insert (Pos 1) 'b'
    , insert (Pos 2) 'c'
    ]

basicInsertRunesTest :: (Editable str, Rune str ~ Char)
                     => CheckStorage backend str m 
                     -> Assertion
basicInsertRunesTest checkStorage = do
  checkStorage (fromString "hello") [
      insert (Pos 0) 'h'
    , insert (Pos 1) 'e'
    , insert (Pos 2) 'l'
    , insert (Pos 3) 'l'
    , insert (Pos 4) 'o'
    ]

basicInsertLineTest :: IsString str
                    => CheckStorage backend str m 
                    -> Assertion
basicInsertLineTest checkStorage =
  checkStorage "hello basic world" [ 
      insertLine (Pos 0)  "hello "
    , insertLine (Pos 6)  "basic "
    , insertLine (Pos 12) "world"
    ]

{------------------------------------------------------------------------------
 Deletion tests
--------------------------------------------------------------------------------}

basicDeleteRuneBeginningTest :: IsString str
                             => CheckStorage backend str m 
                             -> Assertion
basicDeleteRuneBeginningTest checkStorage =
  checkStorage "ello" [ 
      insertLine (Pos 0)  "hello"
    , delete (Pos 0)
    ]

basicDeleteRuneMiddleTest :: IsString str
                    => CheckStorage backend str m 
                    -> Assertion
basicDeleteRuneMiddleTest checkStorage =
  checkStorage "hello" [ 
      insertLine (Pos 0)  "helllo"
    , delete (Pos 3)
    ]

basicDeleteRuneEndTest :: IsString str
                       => CheckStorage backend str m 
                       -> Assertion
basicDeleteRuneEndTest checkStorage =
  checkStorage "hello" [ 
      insertLine (Pos 0)  "hello "
    , delete (Pos 5)
    ]

basicDeleteLineBeginningTest :: IsString str 
                             => CheckStorage backend str m 
                             -> Assertion
basicDeleteLineBeginningTest checkStorage =
  checkStorage "basic world" [ 
      insertLine  (Pos 0)  "hello basic world"
    , deleteRange (Range (Pos 0) (Pos 6))
    ]

basicDeleteLineMiddleTest :: IsString str 
                          => CheckStorage backend str m 
                          -> Assertion
basicDeleteLineMiddleTest checkStorage =
  checkStorage "hello world" [ 
      insertLine  (Pos 0)  "hello basic world"
    , deleteRange (Range (Pos 6) (Pos 12))
    ]

basicDeleteLineEndTest :: IsString str 
                       => CheckStorage backend str m 
                       -> Assertion
basicDeleteLineEndTest checkStorage =
  checkStorage "hello basic" [ 
      insertLine  (Pos 0)  "hello basic world"
    , deleteRange (Range (Pos 11) (Pos 17))
    ]

basicItemAtRuneExistsTest :: ( Monad m
                             , Editable str
                             , Show str
                             , Rune str ~ Char
                             )
                          => CheckResult backend str m 
                          -> Assertion
basicItemAtRuneExistsTest checkResult =
  checkResult (pure $ Just 'l') (itemAt (Pos 3)) [ insertLine (Pos 0) "hello" ]

basicItemAtRuneDoesntExistsTest :: ( Monad m
                                   , Editable str
                                   , Show str
                                   , Show (Rune str)
                                   , Eq (Rune str)
                                   )
                                => CheckResult backend str m 
                                -> Assertion
basicItemAtRuneDoesntExistsTest checkResult =
  checkResult (pure Nothing) (itemAt (Pos 10)) [ insertLine (Pos 0) "hello" ]

basicItemsAtLineExistsTest :: (Monad m, Editable str, Show str)
                           => CheckResult backend str m 
                           -> Assertion
basicItemsAtLineExistsTest checkResult =
  checkResult (pure "llo") (itemsAt (Range (Pos 2) (Pos 4))) [ 
    insertLine (Pos 0) "hello" 
    ]

basicItemsAtLineDoesntExistsTest :: (Monad m, Editable str, Show str)
                                 => CheckResult backend str m 
                                 -> Assertion
basicItemsAtLineDoesntExistsTest checkResult =
  checkResult (pure "") (itemsAt (Range (Pos 0) (Pos 10))) [ 
    insertLine (Pos 0) "hello"
  ]
