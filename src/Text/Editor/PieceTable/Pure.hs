{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E
import Text.Editor.PieceTable.Types

import Control.Monad.Identity

import Data.List (foldl')
import Data.Function
import Data.Coerce
import Data.Map.Strict (Map)
import Data.String
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import qualified Data.ListZipper as Z
import Data.Maybe (fromMaybe)

import qualified Text.Editor.Editable as Editable
import Data.Text (Text)
import Data.ByteString (ByteString)

import Debug.Trace

-- A type tag.
data PieceTable str

insertPiece :: Pos 'Logical -> Piece -> [Piece] -> [Piece]
insertPiece logicalPos newPiece pcs = maybe (pcs <> [newPiece]) id $ do
  z  <- Z.zipper pcs
  z' <- Z.runListZipperOp (Z.moveRightUntil (inRange logicalPos)) $ z
  -- We have to (unfortunately) call 'reverse' on the left part of the zipper
  -- to restore the correct order.
  case z' of
    (Z.ListZipper left focus right, ()) -> do
      pure $ case splitPiece logicalPos focus of
        Before sibling ->
          reverse left <> [newPiece] <> map increaseDistance (sibling : right)
        After sibling ->
          reverse left <> [sibling,newPiece] <> map increaseDistance right
        InBetween this that ->
          reverse left <> [this, newPiece] <> map increaseDistance (that : right)
  where
    increaseDistance :: Piece -> Piece
    increaseDistance = -- increaseRootDistance (pieceLength newPiece)
      increaseRootDistance (getPos (rootDistance newPiece) + pieceLength newPiece)
--
-- Old version of 'insertPiece'
--

--insertPiece :: Pos 'Logical -> Piece -> [Piece] -> [Piece]
--insertPiece logicalPos p pcs =
--  go (Pos 0) pcs []
--  where
--    go :: Pos 'Logical -> [Piece] -> [Piece] -> [Piece]
--    go lPos [] !acc = reverse (p : acc)
--    go lPos (x:xs) acc =
--        let lPos' = lPos + (Pos $ pieceLength x)
--        in if lPos' > logicalPos
--           then case splitPiece (coerce $ logicalPos - lPos) x of
--                  After  this -> p : this : (xs <> acc)
--                  Before this -> this : p : (xs <> acc)
--                  InBetween before after -> before : p : after : (xs <> acc)
--           else go lPos' xs (x : acc)
--

deletePiece :: Range 'Logical -> [Piece] -> [Piece]
deletePiece deleteRange@Range{..} pcs = maybe pcs id $ do
  z  <- Z.zipper pcs
  z' <- flip Z.runListZipperOp z $ do
          Z.moveRightUntil (inRange rStart)
          Z.opUntil Z.deleteStepRight (inRange rEnd)
  case z' of
    (Z.ListZipper left focus right, ()) -> do
      -- Here we have to handle the case where the deletion happens
      -- within the same piece or between pieces.
      if | inRange rStart focus -> do
          pure $ case splitPiece rStart focus of
            Before sibling ->
              reverse left <> map decreaseDistance (amend sibling : right)
            After sibling ->
              reverse left <> [amendFromEnd sibling] <> map decreaseDistance right
            InBetween this that ->
              reverse left <> [this] <> map decreaseDistance (amend that : right)
         | otherwise -> do
          pure $ case splitPiece rEnd focus of
            Before sibling ->
              reverse left <> map decreaseDistance (amend sibling : right)
            After sibling ->
              reverse left <> [amendFromEnd sibling] <> map decreaseDistance right
            InBetween this that ->
              reverse left <> map decreaseDistance (that : right)
  where
    decreaseDistance :: Piece -> Piece
    decreaseDistance = decreaseRootDistance (rangeLength deleteRange)

    amend :: Piece -> Piece
    amend p = p { startPos = startPos p + Pos (rangeLength deleteRange) }

    amendFromEnd :: Piece -> Piece
    amendFromEnd p = p { startPos = endPos p - Pos (rangeLength deleteRange) }


--
-- Old version of 'deletePiece'
--

--deletePiece :: Range 'Logical -> [Piece] -> [Piece]
--deletePiece logicalRange pcs = go (Pos 0) pcs []
--  where
--    go :: Pos 'Logical -> [Piece] -> [Piece] -> [Piece]
--    go lPos [] !acc = reverse acc
--    go lPos (x:xs) acc =
--        let lPos' = lPos + (Pos $ pieceLength x)
--            pPos  = coerce $ (rStart logicalRange - lPos)
--        in if lPos' > rStart logicalRange
--           then case splitPiece pPos x of
--                  InBetween before after ->
--                    let after' = after {
--                            startPos = startPos after + Pos (rangeLength logicalRange)
--                          }
--                      -- Modify after to ignore the deleted range.
--                      -- If doing so would make the piece \"overflow\",
--                      -- we discard it altogether.
--                     in if | isNull before && not (isNull after') -> after' : (xs <> acc)
--                           | isNull after' ->
--                               let newRange = logicalRange {
--                                              rStart = rStart logicalRange + Pos (pieceLength after)
--                                            }
--                               in if | isNull before -> deletePiece newRange (reverse $ before : (xs <> acc))
--                                     | True          -> deletePiece newRange (reverse $ xs <> acc)
--                           | True          -> before : after' : (xs <> acc)
--                  _ -> error "Todo"
--           else go lPos' xs (x : acc)

type instance InternalStorage (PieceTable str) = Storage str

data Storage str where
  MkStorage :: (Show (Rune str), Show str, Editable str) => {
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
    , _deleteRange = \range   -> pure $ self (deleteRangeImpl storage range)
    , _itemAt      = \pos     -> pure $ itemAtImpl storage pos
    , _itemsAt     = \range   -> pure $ itemsAtImpl storage range
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
    newPiece   = Piece AddBuffer (Pos bufLen) (Pos $ bufLen + 1) insertionPoint
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
                                 insertionPoint
    pieces'    = insertPiece insertionPoint newPiece pieces

deleteImpl :: Storage str
           -> Pos 'Logical
           -> Storage str
deleteImpl s deletionPoint =
  deleteRangeImpl s (Range deletionPoint deletionPoint)

deleteRangeImpl :: Storage str
                -> Range 'Logical
                -> Storage str
deleteRangeImpl s@MkStorage{..} deleteRange =
  s { pieces = deletePiece deleteRange pieces }

{-----------------------------------------------------------------------------
  Lookup
------------------------------------------------------------------------------}

itemAtImpl :: Storage str
           -> Pos 'Logical
           -> Maybe (Rune str)
itemAtImpl s@MkStorage{..} lookupPoint = do
  let res = itemsAtImpl s (Range lookupPoint lookupPoint)
  Editable.head res

itemsAtImpl :: forall str. Storage str
            -> Range 'Logical
            -> str
itemsAtImpl storage@MkStorage{..} r@Range{..} = fromMaybe mempty $ do
    z <- Z.zipper pieces
    (start, ()) <- Z.runListZipperOp (Z.moveRightUntil (inRange rStart)) z
    case start of
      Z.ListZipper _ focus rs ->
        let focus' = case splitPiece rStart focus of
              Before x -> x
              After  x -> x
              InBetween _ x -> x
            in pure $ go mempty (rangeLength r) focus' rs
  where
    go :: str -> Int -> Piece -> [Piece] -> str
    go !acc !toRead p ps
      | toRead <= pieceLength p =
          let txt = Editable.take toRead (pieceToStr storage p)
          in acc <> txt
      | otherwise =
          let txt = Editable.take toRead (pieceToStr storage p)
          in case ps of
               [] -> mempty -- not enough bytes
               (x:xs) -> go (acc <> txt) (toRead - (pieceLength p)) x xs

{-----------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

-- | Converts a 'Piece' into a 'str'.
pieceToStr :: Storage str -> Piece -> str
pieceToStr MkStorage{..} Piece{..} =
  let tgt = case source of
              Original  -> originalBuffer
              AddBuffer -> addBuffer
      txt = tgt & Editable.drop (coerce startPos)
                & Editable.take (coerce $ endPos - startPos)
      in txt

-- | Retrieves the /entire/ content of the internal storage by reconstructing
-- the content using the 'Piece's.
debugDumpStorage :: Editor str -> str
debugDumpStorage ts = case _storage ts of
  s@MkStorage{..} -> mconcat $ map (pieceToStr s) pieces


emptyStorage :: (Show (Rune str), Show str, Editable str) => Storage str
emptyStorage = MkStorage mempty mempty mempty

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

pureEditor :: (Show (Rune str), Show str, Editable str) => Editor str
pureEditor = fix mkPureEditor emptyStorage

pureTxtEditor :: Editor Text
pureTxtEditor = fix mkPureEditor emptyStorage

pureBinaryEditor :: Editor ByteString
pureBinaryEditor = fix mkPureEditor emptyStorage

pureStrEditor :: Editor String
pureStrEditor = fix mkPureEditor emptyStorage
