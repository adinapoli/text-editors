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
import qualified Yi.Rope as Yi

class (IsString str, Eq str, Monoid str) => Editable str where
  type family Rune str :: *

  pack      :: [Rune str] -> str
  singleton :: Rune str -> str
  head      :: str -> Maybe (Rune str)
  tail      :: str -> Maybe str
  splitAt   :: Int -> str -> (str, str)
  length    :: str -> Int
  drop      :: Int -> str -> str
  take      :: Int -> str -> str

instance Editable String where
  type instance Rune String = Char

  pack      = id
  singleton = (:[])
  head    s = if L.null s then Nothing else Just $ L.head s
  tail    s = if L.null s then Nothing else Just $ L.tail s
  splitAt   = L.splitAt
  length    = L.length
  take      = L.take
  drop      = L.drop

instance Editable T.Text where
  type instance Rune T.Text = Char

  pack      = T.pack
  singleton = T.singleton
  head    t = if T.null t then Nothing else Just $ T.head t
  tail    t = if T.null t then Nothing else Just $ T.tail t
  splitAt   = T.splitAt
  length    = T.length
  take      = T.take
  drop      = T.drop

instance Editable B.ByteString where
  type instance Rune B.ByteString = Word8

  pack      = B.pack
  singleton = B.singleton
  head    b = if B.null b then Nothing else Just $ B.head b
  tail    b = if B.null b then Nothing else Just $ B.tail b
  splitAt   = B.splitAt
  length    = B.length
  take      = B.take
  drop      = B.drop

instance Editable Yi.YiString where
  type instance Rune Yi.YiString = Char

  pack      = Yi.fromString
  singleton = Yi.singleton
  head      = Yi.head
  tail      = Yi.tail
  splitAt   = Yi.splitAt
  length    = Yi.length
  take      = Yi.take
  drop      = Yi.drop
