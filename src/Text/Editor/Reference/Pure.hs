{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Editor.Reference.Pure (
      Reference
    , mkPureEditor
    , pureEditor
    , pureTxtEditor
    , pureBinaryEditor
    , pureStrEditor
    -- * Debug utility functions
    , debugDumpStorage
    -- * Internals
    , Storage(..)
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E

import Control.Monad.Identity

import Data.Map.Strict (Map)
import Data.Maybe
import Data.String
import qualified Data.Map.Strict as M

import qualified Text.Editor.Editable as Editable
import Data.Text (Text)
import Data.ByteString (ByteString)

-- A type tag.
data Reference str

type instance InternalStorage (Reference str) = Storage str

data Storage str where
  MkStorage :: Editable str => str -> Storage str

type ReferenceEditor str m = TextEditor (Reference str) str m

-- | A purely-functional editor that uses open recursion to propagate the
-- changes to the internal storage.
-- Approach taken from [this blog post](https://www.well-typed.com/blog/2018/03/oop-in-haskell/)
mkPureEditor :: (Editable str, Monad m)
             => (Storage str -> ReferenceEditor str m)
             -> Storage str
             -> ReferenceEditor str m
mkPureEditor self storage = TextEditor
    { _storage     = storage
    , _insert      = \pos c   -> pure $ self (insertImpl storage pos c)
    , _insertLine  = \pos str -> pure $ self (insertLineImpl storage pos str)
    , _delete      = \pos     -> pure $ self (deleteImpl storage pos)
    , _deleteRange = \(Range s e) ->
        pure $ self (foldr (\_ acc -> deleteImpl acc s) storage [s..e])
    , _itemAt     = \(Pos ix) -> pure $
        case storage of
          MkStorage s -> case Editable.splitAt ix s of
                           (_, "") -> Nothing 
                           (_, xs) -> Editable.head xs
    , _itemsAt    = \(Range start end) -> do
        res <- mapM (_itemAt (self storage)) [start .. end]
        case sequence res of
          Nothing -> pure mempty
          Just xs -> pure $ Editable.pack xs
    }

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
                (before, xs) -> 
                    maybe before ((<>) before) (Editable.tail xs)

{-----------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

-- | Retrieves the /entire/ content of the internal storage.
debugDumpStorage :: ReferenceEditor str m -> str
debugDumpStorage ts = case _storage ts of (MkStorage s) -> s

emptyStorage :: Editable str => Storage str
emptyStorage = MkStorage mempty

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

pureEditor :: (Monad m, Editable str) => ReferenceEditor str m
pureEditor = fix mkPureEditor emptyStorage

pureTxtEditor :: Monad m => ReferenceEditor Text m
pureTxtEditor = fix mkPureEditor emptyStorage

pureBinaryEditor :: Monad m => ReferenceEditor ByteString m
pureBinaryEditor = fix mkPureEditor emptyStorage

pureStrEditor :: Monad m => ReferenceEditor String m
pureStrEditor = fix mkPureEditor emptyStorage
