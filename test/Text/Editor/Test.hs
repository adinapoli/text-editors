module Text.Editor.Test  where

import Test.Tasty
import Test.Tasty.HUnit

testEditorTests :: [TestTree]
testEditorTests = 
    [ testCase "basic insert rune" basicInsertRuneTest
    , testCase "basic insert line" basicInsertLineTest
    , testCase "basic delete rune" basicDeleteRuneTest
    , testCase "basic delete line" basicDeleteLineTest
    , testCase "basic itemAt rune exists" basicItemAtRuneExistsTest
    , testCase "basic itemAt rune doesn't exists" basicItemAtRuneDoesntExistsTest
    , testCase "basic itemsAt line exists" basicItemsAtLineExistsTest
    , testCase "basic itemsAt line doesn't exists" basicItemsAtLineDoesntExistsTest
    ]

basicInsertRuneTest :: Assertion
basicInsertRuneTest = pure ()

basicInsertLineTest :: Assertion
basicInsertLineTest = pure ()

basicDeleteRuneTest :: Assertion
basicDeleteRuneTest = pure ()

basicDeleteLineTest :: Assertion
basicDeleteLineTest = pure ()

basicItemAtRuneExistsTest :: Assertion
basicItemAtRuneExistsTest = pure ()

basicItemAtRuneDoesntExistsTest :: Assertion
basicItemAtRuneDoesntExistsTest = pure ()

basicItemsAtLineExistsTest :: Assertion
basicItemsAtLineExistsTest = pure ()

basicItemsAtLineDoesntExistsTest :: Assertion
basicItemsAtLineDoesntExistsTest = pure ()
