{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Editor.Types (
    -- * Types
      PosType(..)
    , Pos(..)
    , Range(..)
    , InternalStorage
    , TextEditorAPI(..)
    , TextEditor(..)
    -- * Main API
    , load
    , save
    , insert
    , insertLine
    , delete
    , deleteRange
    , itemAt
    , itemsAt
    -- * Internals
    , rangeLength
    ) where

import GHC.TypeLits
import Control.Monad
import Data.Monoid
import Data.Word
import Data.Coerce
import Data.Function ((&))
import Data.String
import Data.ByteString (ByteString)
import Data.Text (Text)

import Text.Editor.Editable

data PosType = Logical | Physical

-- | A position identified by the X and Y coords.
newtype Pos (ty :: PosType) = Pos { getPos :: Int }
    deriving (Show, Eq, Ord, Enum, Num)

instance Semigroup (Pos 'Logical) where
    (<>) (Pos p1) (Pos p2) = Pos (p1 + p2)

instance Monoid (Pos 'Logical) where
    mappend = (<>)
    mempty  = Pos 0

type family InternalStorage (backend :: *) :: *

data TextEditorAPI backend str (m :: * -> *) = TextEditorAPI
  { _load :: FilePath -> (forall r. (TextEditor backend str m -> m r) -> m r)
  -- ^ Load the content of a file and /produces/ a 'TextEditor' in a
  -- \"bracketed\" form.
  , _save :: FilePath -> TextEditor backend str m -> m ()
  -- ^ Save the content of a 'TextEditor'.
  }

load :: FilePath
     -> TextEditorAPI backend str m
     -> (forall r. (TextEditor backend str m -> m r) -> m r)
load fp api = _load api $ fp

save :: FilePath
     -> TextEditor    backend str m
     -> TextEditorAPI backend str m
     -> m ()
save fp ed api = (_save api) fp ed

data Range (ty :: PosType) =
    Range { rStart :: Pos ty
          -- ^ The /inclusive/ start of the 'Range'.
          , rEnd   :: Pos ty
          -- ^ The /inclusive/ end of the 'Range'.
          } deriving Show

-- | Returns the length of the range. Being both ends /inclusive/ means
-- that 'rangeLength (Pos 0) (Pos 0) === 1'.
rangeLength :: Range ty -> Int
rangeLength r = coerce (rEnd r - rStart r) + 1

-- | A 'TextEditor' generic interface implemented as a record
-- of functions.
data TextEditor backend str (m :: * -> *)
  = TextEditor
      { _storage     :: InternalStorage backend
      , _insert      :: Pos 'Logical -> Rune str -> m (TextEditor backend str m)
      , _insertLine  :: Pos 'Logical -> str -> m (TextEditor backend str m)
      , _delete      :: Pos 'Logical -> m (TextEditor backend str m)
      , _deleteRange :: Range 'Logical -> m (TextEditor backend str m)
      , _itemAt      :: Pos 'Logical   -> m (Maybe (Rune str))
      , _itemsAt     :: Range 'Logical -> m str
      }

insert :: Pos 'Logical
       -> Rune str
       -> TextEditor backend str m
       -> m (TextEditor backend str m)
insert pos rune old = _insert old pos rune

insertLine :: Pos 'Logical
           -> str
           -> TextEditor backend str m
           -> m (TextEditor backend str m)
insertLine pos str old = _insertLine old pos str

delete :: Pos 'Logical
       -> TextEditor backend str m
       -> m (TextEditor backend str m)
delete pos old = _delete old pos

deleteRange :: Range 'Logical
            -> TextEditor backend str m
            -> m (TextEditor backend str m)
deleteRange r old = _deleteRange old r

itemAt :: Pos 'Logical
       -> TextEditor backend str m
       -> m (Maybe (Rune str))
itemAt pos old = _itemAt old pos

itemsAt :: Range 'Logical
        -> TextEditor backend str m
        -> m str
itemsAt r old = _itemsAt old r

exampleUserSession :: Monad m
                   => TextEditorAPI backend String m
                   -> m ()
exampleUserSession api = do
  load "text.txt" api $ \edtr -> do
    edtr' <-     insertLine (Pos 0) "foo"
             >=> insertLine (Pos 0) "bar"
             >=> insert     (Pos 2) 'a'
             >=> delete     (Pos 1)
             $ edtr

    save "text.txt" edtr' api

{- A monadic API would feel natural:

monadicAPI
monadicAPI = do
  load "text.txt"
  insertLine (Pos 0) "foo"
  insertLine (Pos 0) "bar"
  insert     (Pos 2) 'a'
  deleteLine (Pos 1)
  x <- itemAt (Pos 2)
  save "out.txt"
  pure x

But that would imply deriving also an instance for Applicative and Functor,
but that would make little sense: what does it mean to 'fmap' over a Text
Editor?

-}
