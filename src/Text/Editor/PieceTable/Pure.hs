{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Text.Editor.PieceTable.Pure (
    PieceTable
    , pureEditor
    , pureTxtEditor
    , pureBinaryEditor
    , pureStrEditor
    -- * Debug utility functions
    , debugDumpStorage
    -- * Internals
    , splitPiece
    , Piece(..)
    , Source(..)
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E

import Control.Monad.Identity

import Data.Function
import Data.Coerce
import Data.Map.Strict (Map)
import Data.String
import qualified Data.Map.Strict as M

import qualified Text.Editor.Editable as Editable
import Data.Text (Text)
import Data.ByteString (ByteString)

-- A type tag.
data PieceTable str

data Source =
    Original
  | AddBuffer
  deriving Show

-- | A 'Piece'.
data Piece = Piece {
    source :: Source
  , startPos :: Pos 'Physical
  , endPos   :: Pos 'Physical
  } deriving Show

pieceLength :: Piece -> Int
pieceLength Piece{..} = coerce (endPos - startPos)

-- | Builds a new 'Piece' out of an input 'Editable' string.
pieceFromStr :: Editable str => str -> Source -> Piece
pieceFromStr str s = Piece s (Pos 0) (Pos $ Editable.length str)

-- | Returns 'True' if the input 'Pos' falls within the input 'Piece'.
inRange :: Pos 'Physical -> Piece -> Bool
inRange pos Piece{..} = startPos <= pos && pos <= endPos

splitPiece :: Pos 'Physical -> Piece -> (Piece, Piece)
splitPiece pos piece@Piece{..}
  | inRange pos piece = ( Piece source startPos pos
                        , Piece source pos endPos
                        )
  | otherwise = error "splitPiece called on an invalid range."

insertPiece :: Pos 'Logical -> Piece -> [Piece] -> [Piece]
insertPiece logicalPos p pcs = go (Pos 0) pcs []
  where
    go :: Pos 'Logical -> [Piece] -> [Piece] -> [Piece]
    go lPos [] !acc = reverse (p : acc)
    go lPos (x:xs) acc =
        let lPos' = lPos + (Pos $ pieceLength x)
         in if lPos' >= logicalPos
            then case splitPiece (coerce $ logicalPos - lPos) x of
                   (before, after) -> before : p : after : (xs <> acc)
            else go lPos' xs (x : acc)

type instance InternalStorage (PieceTable str) = Storage str

data Storage str where
  MkStorage :: Editable str => {
      originalBuffer :: str
    , addBuffer      :: str
    , pieces         :: [Piece]
    } -> Storage str

deriving instance Show str => Show (Storage str)

type Editor str = TextEditor (PieceTable str) str Identity

-- | A purely-functional editor that uses open recursion to propagate the
-- changes to the internal storage.
-- Approach taken from [this blog post](https://www.well-typed.com/blog/2018/03/oop-in-haskell/)
mkPureEditor :: Editable str 
             => (Storage str -> Editor str)
             -> Storage str
             -> Editor str
mkPureEditor self storage = TextEditor
    { _storage     = storage
    , _insert      = \pos c   -> pure $ self (insertImpl storage pos c)
    , _insertLine  = \pos str -> pure $ self (insertLineImpl storage pos str)
    , _delete      = \pos     -> pure $ self (deleteImpl storage pos)
    , _deleteRange = \(Range s e) ->
        pure $ self (foldr (\_ acc -> deleteImpl acc s) storage [s..e])
    , _itemAt     = \(Pos ix) -> pure $ undefined
    , _itemsAt    = undefined
    }

insertImpl :: Storage str
           -> Pos 'Logical
           -> Rune str 
           -> Storage str
insertImpl s@MkStorage{..} insertionPoint rune =
  s { addBuffer = addBuffer'
    , pieces    = pieces'
    }
  where
    addBuffer' = addBuffer <> Editable.singleton rune
    bufLen     = Editable.length $ addBuffer
    newPiece   = Piece AddBuffer (Pos bufLen) (Pos $ bufLen + 1)
    pieces'    = insertPiece insertionPoint newPiece pieces


insertLineImpl :: Storage str
               -> Pos 'Logical
               -> str 
               -> Storage str
insertLineImpl s@MkStorage{..} insertionPoint line =
  s { addBuffer = addBuffer'
    , pieces    = pieces'
    }
  where
    addBuffer' = addBuffer <> line
    bufLen     = Editable.length $ addBuffer
    newPiece   = Piece AddBuffer (Pos bufLen) 
                                 (Pos $ bufLen + Editable.length line)
    pieces'    = insertPiece insertionPoint newPiece pieces

deleteImpl :: Storage str
           -> Pos 'Logical
           -> Storage str
deleteImpl s (Pos ix) = undefined

{-----------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

-- | Retrieves the /entire/ content of the internal storage by reconstructing
-- the content using the 'Piece's.
debugDumpStorage :: Editor str -> str
debugDumpStorage ts = case _storage ts of
  MkStorage{..} -> foldr f mempty pieces
  where
    f Piece{..} acc = 
      case _storage ts of
        MkStorage{..} ->
          let tgt = case source of
                      Original  -> originalBuffer
                      AddBuffer -> addBuffer
              txt = tgt & Editable.drop (coerce startPos)
                        & Editable.take (coerce $ endPos - startPos)
              in txt <> acc


emptyStorage :: Editable str => Storage str
emptyStorage = MkStorage mempty mempty mempty

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

pureEditor :: Editable str => Editor str
pureEditor = fix mkPureEditor emptyStorage

pureTxtEditor :: Editor Text
pureTxtEditor = fix mkPureEditor emptyStorage

pureBinaryEditor :: Editor ByteString
pureBinaryEditor = fix mkPureEditor emptyStorage

pureStrEditor :: Editor String
pureStrEditor = fix mkPureEditor emptyStorage
