{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Editor.Types where

import Data.Word
import Data.Function ((&))
import Data.String
import Data.ByteString (ByteString)
import Data.Text (Text)

import Text.Editor.Editable

data PosType = Logical | Physical

-- | A position identified by the X and Y coords.
newtype Pos (ty :: PosType) = Pos Int

type family InternalStorage (backend :: *) :: *

data TextEditorAPI backend str (m :: * -> *) = TextEditorAPI
  { load :: FilePath -> m (TextEditor backend str m)
  -- ^ Load the content of a file and /produces/ a 'TextEditor'.
  , save :: FilePath -> TextEditor backend str m -> m ()
  -- ^ Save the content of 'TextEditor' /consuming/ it.
  }

-- | A 'TextEditor' generic interface implemented as a record
-- of functions.
data TextEditor backend str (m :: * -> *)
  = TextEditor
      { _storage    :: InternalStorage backend
      , _insert     :: Pos 'Logical -> Rune str -> TextEditor backend str m
      , _insertLine :: Pos 'Logical -> str -> TextEditor backend str m
      , _delete     :: Pos 'Logical -> TextEditor backend str m
      , _deleteLine :: Pos 'Logical -> TextEditor backend str m
      , _itemAt     :: Pos 'Logical -> m (Maybe (Rune str))
      , _itemsAt    :: Pos 'Logical -> m str
      }

insert :: Pos 'Logical
       -> Rune str
       -> TextEditor backend str m 
       -> TextEditor backend str m
insert pos rune old = _insert old pos rune

insertLine :: Pos 'Logical
           -> str
           -> TextEditor backend str m 
           -> TextEditor backend str m
insertLine pos str old = _insertLine old pos str

delete :: Pos 'Logical
       -> TextEditor backend str m 
       -> TextEditor backend str m
delete pos old = _delete old pos

deleteLine :: Pos 'Logical
           -> TextEditor backend str m 
           -> TextEditor backend str m
deleteLine pos old = _deleteLine old pos

exampleUserSession :: Monad m 
                   => TextEditorAPI backend String m 
                   -> m ()
exampleUserSession api = do
  edtr <- load api "text.txt"
  save api "text.txt" $ edtr & insertLine (Pos 0) "foo"
                             . insertLine (Pos 0) "bar"
                             . insert     (Pos 2) 'a'
                             . deleteLine (Pos 1)
