{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Text.Editor.PieceTable.IO (
      PieceTable
    , ioEditorAPI
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E
import Text.Editor.PieceTable.Types

import Foreign.Ptr

import Control.Exception

import Data.List (foldl')
import Data.Function
import Data.Coerce
import Data.Map.Strict (Map)
import Data.String
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import qualified Data.ListZipper as Z
import Data.Maybe (fromMaybe)

import qualified Text.Editor.Editable as Editable
import Data.Text (Text)
import Data.ByteString (ByteString)

import System.IO.MMap as MMap

-- A type tag.
data PieceTable

type instance InternalStorage PieceTable = Storage

data Storage = MkStorage {
    originalBufferPtr     :: Ptr Char
  , originalBufferRawSize :: !Int
  , originalBufferOffset  :: !Int
  , originalBufferSize    :: !Int
  , addBuffer             :: Text
  , pieces                :: [Piece]
  } deriving Show

type Editor = TextEditor PieceTable Text IO

mkIoEditor :: Storage -> Editor
mkIoEditor storage = TextEditor
    { _storage     = storage
    , _insert      = undefined
    , _insertLine  = undefined
    , _delete      = undefined
    , _deleteRange = undefined
    , _itemAt      = undefined
    , _itemsAt     = undefined
    }


{-----------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

ioEditorAPI :: TextEditorAPI PieceTable Text IO
ioEditorAPI = TextEditorAPI {
  load = \fp -> do
    bracket (acquire fp) dispose $ \(ptr, rs, offset, sz) -> do
      let initialStorage = MkStorage {
            originalBufferPtr     = ptr
          , originalBufferRawSize = rs
          , originalBufferOffset  = offset
          , originalBufferSize    = sz
          , addBuffer             = mempty
          , pieces                = mempty
          }
      pure $ mkIoEditor initialStorage
  }
  where
    acquire :: FilePath -> IO (Ptr Char, Int, Int, Int)
    acquire fp = MMap.mmapFilePtr fp MMap.ReadOnly Nothing
    
    dispose :: (Ptr Char, Int, Int, Int) -> IO ()
    dispose (ptr, rs, _,_) = MMap.munmapFilePtr ptr rs

