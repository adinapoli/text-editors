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

newtype Row = Row { getRow :: Int }
newtype Col = Col { getCol :: Int }

-- | A position identified by the X and Y coords.
data Pos = Pos { row :: !Row, col :: !Col }

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
      { _storage   :: InternalStorage backend
      , _insert     :: Pos -> Rune str -> TextEditor backend str m
      , _insertLine :: Row -> str -> TextEditor backend str m
      , _delete     :: Pos -> TextEditor backend str m
      , _deleteLine :: Row -> TextEditor backend str m
      }

insert :: Pos 
       -> Rune str
       -> TextEditor backend str m 
       -> TextEditor backend str m
insert pos rune old = _insert old pos rune

insertLine :: Row
           -> str
           -> TextEditor backend str m 
           -> TextEditor backend str m
insertLine row str old = _insertLine old row str

delete :: Pos
       -> TextEditor backend str m 
       -> TextEditor backend str m
delete pos old = _delete old pos

deleteLine :: Row
           -> TextEditor backend str m 
           -> TextEditor backend str m
deleteLine row old = _deleteLine old row

exampleUserSession :: Monad m 
                   => TextEditorAPI backend String m 
                   -> m ()
exampleUserSession api = do
  edtr <- load api "text.txt"
  save api "text.txt" $ edtr & insertLine (Row 0) "foo"
                             . insertLine (Row 0) "bar"
                             . insert     (Pos (Row 1) (Col 5)) 'a'
                             . deleteLine (Row 0)

