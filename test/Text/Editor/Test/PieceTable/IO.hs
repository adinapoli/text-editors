{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Text.Editor.Test.PieceTable.IO where

import Control.Monad.Identity
import Data.Proxy
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.PieceTable.IO
import Text.Editor.Types
import Text.Editor.Test

checkEmptyStorage :: CheckStorage PieceTable Text IO
checkEmptyStorage = \expected actions -> do
  editor <- load "data-files/empty.txt" ioEditorAPI
  checkApi (pure editor) 
           id
           (debugDumpStorage) 
           (pure expected)
           actions

checkResult :: CheckResult PieceTable Text IO
checkResult expected fromEditor = \ops -> do
  editor <- load "data-files/empty.txt" ioEditorAPI
  checkApi (pure editor) id fromEditor expected ops

tests :: TestTree
tests = testEditorTests "PieceTable.IO tests"
        (Proxy @PieceTable)
        (Proxy @Text)
        (Proxy @IO)
        checkEmptyStorage
        checkResult
