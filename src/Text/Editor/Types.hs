{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Editor.Types where

import Data.Word
import Data.Function ((&))
import Data.String
import Data.ByteString (ByteString)
import Data.Text (Text)

-- | Extracts the \"rune\" (i.e. the \"element type\")
-- out of a string-like type.
type family Rune (str :: *) :: * where
  Rune String = Char
  Rune ByteString = Word8
  Rune Text = Word16

-- A position identified by the X and Y coords.
data Pos = Pos { x :: !Int, y :: !Int }

type family InternalStorage (backend :: *) :: *

-- | A 'TextEditor' generic interface implemented as a record
-- of functions.
data TextEditor backend str m
  = TextEditor
      { _storage   :: InternalStorage backend
      , _insert     :: Pos -> Rune str -> TextEditor backend str m
      , _insertLine :: Pos -> str  -> TextEditor backend str m
      , _delete     :: Pos -> TextEditor backend str m
      , _deleteLine :: Pos -> TextEditor backend str m
      }

insert :: Pos 
       -> Rune str
       -> TextEditor backend str m 
       -> TextEditor backend str m
insert pos rune old = _insert old pos rune

insertLine :: Pos 
           -> str
           -> TextEditor backend str m 
           -> TextEditor backend str m
insertLine pos str old = _insertLine old pos str

delete :: Pos
       -> TextEditor backend str m 
       -> TextEditor backend str m
delete pos old = _delete old pos

deleteLine :: Pos
           -> TextEditor backend str m 
           -> TextEditor backend str m
deleteLine pos old = _deleteLine old pos


exampleUserSession :: TextEditor backend String m 
                   -> TextEditor backend String m
exampleUserSession edtr = edtr & insertLine (Pos 0 0) "foo"
                               . insertLine (Pos 0 0) "bar"
                               . insert     (Pos 1 5) 'a'
                               . deleteLine (Pos 0 0)

