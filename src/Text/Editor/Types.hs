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
import Data.Word
import Data.Coerce
import Data.Function ((&))
import Data.String
import Data.ByteString (ByteString)
import Data.Text (Text)

import Text.Editor.Editable

data PosType = Logical | Physical

-- | A position identified by the X and Y coords.
newtype Pos (ty :: PosType) = Pos Int deriving (Show, Eq, Ord, Enum, Num)

type family InternalStorage (backend :: *) :: *

data TextEditorAPI backend str (m :: * -> *) = TextEditorAPI
  { load :: FilePath -> m (TextEditor backend str m)
  -- ^ Load the content of a file and /produces/ a 'TextEditor'.
  , save :: FilePath -> TextEditor backend str m -> m ()
  -- ^ Save the content of a 'TextEditor'.
  }

data Range (ty :: PosType) = 
    Range { rStart :: Pos ty
          , rEnd   :: Pos ty
          } deriving Show

rangeLength :: Range ty -> Int
rangeLength r = coerce (rEnd r - rStart r)

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
  edtr <- load api "text.txt"

  edtr' <-     insertLine (Pos 0) "foo"
           >=> insertLine (Pos 0) "bar"
           >=> insert     (Pos 2) 'a'
           >=> delete     (Pos 1)
           $ edtr

  save api "text.txt" edtr'

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
