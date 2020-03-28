{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Editor.Reference.Pure (
    Reference
    , pureEditor
    , pureTxtEditor
    , pureBinaryEditor
    , pureStrEditor
    -- * Debug utility functions
    , debugDumpStorage
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E

import Control.Monad.Identity

import Data.Map.Strict (Map)
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
mkPureEditor :: Editable str 
             => (Storage str -> ReferenceEditor str Identity)
             -> Storage str
             -> ReferenceEditor str Identity
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
                           (_, xs) -> Just (Editable.head xs)
    , _itemsAt    = undefined
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
                (before, xs) -> before <> Editable.tail xs

{-----------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

-- | Retrieves the /entire/ content of the internal storage.
debugDumpStorage :: ReferenceEditor str Identity -> str
debugDumpStorage ts = case _storage ts of (MkStorage s) -> s

emptyStorage :: Editable str => Storage str
emptyStorage = MkStorage mempty

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

pureEditor :: Editable str => ReferenceEditor str Identity
pureEditor = fix mkPureEditor emptyStorage

pureTxtEditor :: ReferenceEditor Text Identity
pureTxtEditor = fix mkPureEditor emptyStorage

pureBinaryEditor :: ReferenceEditor ByteString Identity
pureBinaryEditor = fix mkPureEditor emptyStorage

pureStrEditor :: ReferenceEditor String Identity
pureStrEditor = fix mkPureEditor emptyStorage
