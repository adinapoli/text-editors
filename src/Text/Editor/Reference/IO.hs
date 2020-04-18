{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Editor.Reference.IO (
      Reference
    , ioEditorAPI
    , ioEditor

    -- * Debug utility functions
    , debugDumpStorage
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E

import Control.Monad.Identity

import Data.Map.Strict (Map)
import Data.String
import qualified Data.Map.Strict as M

import qualified Data.Text as T
import Data.Text (Text)

import Data.Vector.Storable.MMap
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable as V

-- A type tag.
data Reference

type instance InternalStorage Reference = IOVector Char

-- | A purely-functional editor that uses open recursion to propagate the
-- changes to the internal storage.
-- Approach taken from [this blog post](https://www.well-typed.com/blog/2018/03/oop-in-haskell/)
mkIoEditor :: InternalStorage Reference -> TextEditor Reference Text IO
mkIoEditor initialStorage = TextEditor
    { _storage     = initialStorage
    , _insert      = undefined
    , _insertLine  = undefined
    , _delete      = undefined
    , _deleteRange = undefined
    , _itemAt      = undefined
    , _itemsAt     = undefined
    }

{-
insertImpl :: Storage  str
           -> Pos 'Logical
           -> Rune str 
           -> Storage str
insertImpl (MkStorage s) (Pos ix) rune = MkStorage $
  case Editable.splitAt ix s of
    (before, rest) -> before <> singleton rune <> rest

insertLineImpl :: Storage str
               -> Pos 'Logical
               -> str 
               -> Storage str
insertLineImpl (MkStorage s) (Pos ix) line =
  MkStorage $ case Editable.splitAt ix s of
                (before, rest) -> before <> line <> rest

deleteImpl :: Storage str
           -> Pos 'Logical
           -> Storage str
deleteImpl (MkStorage s) (Pos ix) =
  MkStorage $ case Editable.splitAt ix s of
                (before, "") -> before
                (before, xs) -> before <> Editable.tail xs
-}

{-----------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

-- | Retrieves the /entire/ content of the internal storage.
debugDumpStorage :: TextEditor Reference Text IO -> IO Text
debugDumpStorage e = T.pack . V.toList <$> (V.freeze $ _storage e)

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

-- | Creates a new IO editor with a default 1024 elements buffer.
ioEditor :: IO (TextEditor Reference Text IO)
ioEditor = do
  initialStorage <- V.unsafeThaw (V.generate 1024 (const '0'))
  pure $ mkIoEditor initialStorage

ioEditorAPI :: TextEditorAPI Reference Text IO
ioEditorAPI = TextEditorAPI {
  _load = \fp -> do
    initialStorage <- unsafeMMapMVector fp ReadWriteEx Nothing
    pure $ mkIoEditor initialStorage
  }
