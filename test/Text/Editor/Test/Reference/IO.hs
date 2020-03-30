{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Text.Editor.Test.Reference.IO where

import Data.Proxy
import Data.Text

import Test.Tasty
import Test.Tasty.HUnit

import Text.Editor.Reference.IO
import Text.Editor.Types
import Text.Editor.Test

checkStorage :: CheckStorage Reference Text IO
checkStorage expected = 
  checkApi ioEditor id debugDumpStorage (pure expected)

checkResult :: CheckResult Reference Text IO
checkResult expected fromEditor = 
  checkApi ioEditor id fromEditor expected

tests :: TestTree
tests = testEditorTests "Reference.IO tests"
        (Proxy @Reference)
        (Proxy @Text)
        (Proxy @IO)
        checkStorage
        checkResult
