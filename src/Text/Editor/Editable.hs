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
  zip     :: str -> str -> [(Rune str, Rune str)]
  find    :: (Rune str -> Bool) -> str -> Maybe (Rune str)
  length  :: str -> Int
  drop    :: Int -> str -> str
  take    :: Int -> str -> str

instance Editable String where
  type instance Rune String = Char

  singleton = (:[])
  head = L.head
  tail = L.tail
  zip = L.zip
  splitAt = L.splitAt
  find = L.find
  length = L.length
  take   = L.take
  drop   = L.drop

instance Editable T.Text where
  type instance Rune T.Text = Char

  singleton = T.singleton
  head = T.head
  tail = T.tail
  zip = T.zip
  splitAt = T.splitAt
  find = T.find
  length = T.length
  take   = T.take
  drop   = T.drop

instance Editable B.ByteString where
  type instance Rune B.ByteString = Word8

  singleton = B.singleton
  head = B.head
  tail = B.tail
  zip = B.zip
  splitAt = B.splitAt
  find = B.find
  length = B.length
  take   = B.take
  drop   = B.drop
