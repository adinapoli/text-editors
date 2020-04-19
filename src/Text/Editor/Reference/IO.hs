{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Editor.Reference.IO (
      ioEditorAPI
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E

import Control.Monad.Identity
import System.IO.Posix.MMap

import Data.String

import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Text (Text)

import qualified Text.Editor.Reference.Pure as Pure

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

ioEditorAPI :: TextEditorAPI (Pure.Reference Text) Text IO
ioEditorAPI = TextEditorAPI {
    _load = \fp act -> do
      blob <- TE.decodeUtf8 <$> unsafeMMapFile fp
      let initialStorage = Pure.MkStorage blob
      act (fix Pure.mkPureEditor initialStorage)
  , _save = \fp editor -> 
      case _storage editor of Pure.MkStorage txt -> TIO.writeFile fp txt
  }
