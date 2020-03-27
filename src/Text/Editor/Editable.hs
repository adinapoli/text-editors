{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Editor.Editable where

import Data.Word
import Data.String
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString as B

class IsString str => Editable str where
  type family Rune str :: *

  zip :: str -> str -> [(Rune str, Rune str)]
  singleton :: Rune str -> str

instance Editable String where
  type instance Rune String = Char

  zip = L.zip
  singleton = (:[])

instance Editable T.Text where
  type instance Rune T.Text = Char

  zip = T.zip
  singleton = T.singleton

instance Editable B.ByteString where
  type instance Rune B.ByteString = Word8

  zip = B.zip
  singleton = B.singleton
