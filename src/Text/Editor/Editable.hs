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

class (IsString str, Eq str, Monoid str) => Editable str where
  type family Rune str :: *

  singleton :: Rune str -> str
  head    :: str -> Rune str
  tail    :: str -> str
  splitAt :: Int -> str -> (str, str)
  zip :: str -> str -> [(Rune str, Rune str)]

instance Editable String where
  type instance Rune String = Char

  singleton = (:[])
  head = L.head
  tail = L.tail
  zip = L.zip
  splitAt = L.splitAt

instance Editable T.Text where
  type instance Rune T.Text = Char

  singleton = T.singleton
  head = T.head
  tail = T.tail
  zip = T.zip
  splitAt = T.splitAt

instance Editable B.ByteString where
  type instance Rune B.ByteString = Word8

  singleton = B.singleton
  head = B.head
  tail = B.tail
  zip = B.zip
  splitAt = B.splitAt
