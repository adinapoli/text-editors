{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Text.Editor.Test.Reference.Yi where

import Control.Monad.Identity
import Data.Proxy
import Yi.Rope

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Reference.Pure
import Text.Editor.Types
import Text.Editor.Test

checkPureStorage :: CheckStorage (Reference YiString) YiString Identity
checkPureStorage expected =
  checkApi (pure pureEditor)
           (pure . runIdentity)
           (pure . debugDumpStorage)
           (pure expected)

checkResult :: CheckResult (Reference YiString) YiString Identity
checkResult expected fromEditor =
  checkApi (pure pureEditor) (pure . runIdentity) fromEditor expected

tests :: TestTree
tests = testEditorTests "Reference.Yi tests"
        (Proxy @(Reference YiString))
        (Proxy @YiString)
        (Proxy @Identity)
        checkPureStorage
        checkResult
