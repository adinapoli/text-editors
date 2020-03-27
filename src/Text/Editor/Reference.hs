{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Editor.Reference where

import Text.Editor.Types
import Text.Editor.Editable as E

import Control.Monad.Identity

import Data.Map.Strict (Map)
import Data.String
import qualified Data.Map.Strict as M

import qualified Text.Editor.Editable as Editable

data Reference str

--data instance InternalStorage (Reference str) = 
--  Storage where
--      MkStorage :: Editable str => Map Row (Map Col (Rune str))

type instance InternalStorage (Reference str) = Storage str

data Storage str where
  MkStorage :: Editable str 
            => str
            -> Storage str

modifyStorage :: Editable str => (str -> str) -> Storage str -> Storage str
modifyStorage f (MkStorage s) = MkStorage $ f s

type ReferenceEditor str m = TextEditor (Reference str) str m

pureEditor :: Editable str => ReferenceEditor str Identity
pureEditor = TextEditor
    { _storage     = MkStorage mempty
    , _insert      = \pos c   -> pure (insertImpl pureEditor pos c)
    , _insertLine  = \pos str -> pure (insertLineImpl pureEditor pos str)
    , _delete      = \pos     -> pure (deleteImpl     pureEditor pos)
    , _deleteRange = \(Range s e) ->
        pure $ foldr (\_ acc -> deleteImpl acc s) pureEditor [s..e]
    , _itemAt     = undefined
    , _itemsAt    = undefined
    }

insertImpl :: forall str. Editable str 
           => ReferenceEditor str Identity
           -> Pos 'Logical
           -> Rune str 
           -> ReferenceEditor str Identity
insertImpl old (Pos ix) rune =
    old { _storage = insertChar (_storage old) }
  where
    insertChar (MkStorage s) = 
      MkStorage $ case Editable.splitAt ix s of
                    (before, rest) -> before <> singleton rune <> rest

insertLineImpl :: ReferenceEditor str Identity
               -> Pos 'Logical
               -> str 
               -> ReferenceEditor str Identity
insertLineImpl old (Pos ix) line =
    old { _storage = insertStr (_storage old)
        }
  where
    insertStr (MkStorage s) = 
      MkStorage $ case Editable.splitAt ix s of
                    (before, rest) -> before <> line <> rest

deleteImpl :: ReferenceEditor str Identity
           -> Pos 'Logical
           -> ReferenceEditor str Identity
deleteImpl old (Pos ix) =
    old { _storage = deleteStr (_storage old) }
  where
    deleteStr (MkStorage s) = 
      MkStorage $ case Editable.splitAt ix s of
                    (before, "") -> before
                    (before, xs) -> before <> Editable.tail xs
