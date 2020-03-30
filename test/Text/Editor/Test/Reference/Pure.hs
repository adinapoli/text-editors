{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Text.Editor.Test.Reference.Pure where

import Control.Monad.Identity
import Data.Proxy

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Reference.Pure
import Text.Editor.Types
import Text.Editor.Test

checkPureStorage :: CheckStorage (Reference String) String Identity
checkPureStorage expected = 
  checkApi (pure pureStrEditor) 
           (pure . runIdentity) 
           (pure . debugDumpStorage) 
           (pure expected)

checkResult :: CheckResult (Reference String) String Identity
checkResult expected fromEditor = 
  checkApi (pure pureStrEditor) (pure . runIdentity) fromEditor expected

tests :: TestTree
tests = testEditorTests "Reference.Pure tests"
        (Proxy @(Reference String))
        (Proxy @String)
        (Proxy @Identity)
        checkPureStorage
        checkResult
