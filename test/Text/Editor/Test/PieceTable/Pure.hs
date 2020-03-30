{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Text.Editor.Test.PieceTable.Pure where

import Control.Monad.Identity
import Data.Proxy

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.PieceTable.Pure
import Text.Editor.Types
import Text.Editor.Test

checkPureStorage :: CheckStorage (PieceTable String) String Identity
checkPureStorage expected = 
  checkApi (pure pureStrEditor) 
           (pure . runIdentity) 
           (pure . debugDumpStorage) 
           (pure expected)

checkResult :: CheckResult (PieceTable String) String Identity
checkResult expected fromEditor = 
  checkApi (pure pureStrEditor) (pure . runIdentity) fromEditor expected

tests :: TestTree
tests = testEditorTests "PieceTable.Pure tests"
        (Proxy @(PieceTable String))
        (Proxy @String)
        (Proxy @Identity)
        checkPureStorage
        checkResult
