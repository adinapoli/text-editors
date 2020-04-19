{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Text.Editor.Test.Reference.IO where

import Data.Proxy
import Data.Text

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Reference.Pure
import Text.Editor.Reference.IO
import Text.Editor.Types
import Text.Editor.Test

checkEmptyStorage :: CheckStorage (Reference Text) Text IO
checkEmptyStorage = \expected actions -> do
  load "data-files/empty.txt" ioEditorAPI $ \editor ->
    checkApi (pure editor) id (pure . debugDumpStorage) (pure expected) actions

checkResult :: CheckResult (Reference Text) Text IO
checkResult expected fromEditor = \ops -> do
  load "data-files/empty.txt" ioEditorAPI $ \editor ->
    checkApi (pure editor) id fromEditor expected ops

tests :: TestTree
tests = testEditorTests "Reference.IO tests"
        (Proxy @(Reference Text))
        (Proxy @Text)
        (Proxy @IO)
        checkEmptyStorage
        checkResult
