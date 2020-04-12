{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
module Text.Editor.PieceTable.IO (
      PieceTable
    , ioEditorAPI
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E
import Text.Editor.PieceTable.Types

import Foreign.Ptr

import Control.Exception

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.FingerTree as FT
import Data.List (foldl')
import Data.Function
import Data.Coerce
import Data.String
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Unsafe as BS
import Data.Word (Word8)
import Control.Monad.State.Strict
import qualified Data.ListZipper as Z
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import Data.ByteString (ByteString)

import System.IO.MMap as MMap

-- A type tag.
data PieceTable

type instance InternalStorage PieceTable = Storage

data Storage = MkStorage {
    originalBufferPtr     :: Ptr Word8
  , originalBufferRawSize :: !Int
  , originalBufferOffset  :: !Int
  , originalBufferSize    :: !Int
  , addBuffer             :: Text
  , pieces                :: FingerTree (Pos 'Logical) Piece
  } deriving Show

type Editor = TextEditor PieceTable Text IO

mkIoEditor :: (Storage -> Editor) 
           -> Storage 
           -> Editor
mkIoEditor self storage = TextEditor
    { _storage     = storage
    , _insert      = \pos c   -> self <$> insertImpl storage pos c
    , _insertLine  = \pos txt -> self <$> insertLineImpl storage pos txt
    , _delete      = \pos     -> self <$> deleteImpl storage pos
    , _deleteRange = \range   -> self <$> deleteRangeImpl storage range
    , _itemAt      = \pos     -> itemAtImpl storage pos
    , _itemsAt     = \range   -> itemsAtImpl storage range
    }

insertImpl :: Storage 
           -> Pos 'Logical 
           -> Char 
           -> IO Storage
insertImpl s@MkStorage{..} insertionPoint rune = pure $
  s { addBuffer = addBuffer'
    , pieces    = pieces'
    }
  where
    addBuffer' = addBuffer <> T.singleton rune
    bufLen     = T.length addBuffer
    newPiece   = Piece AddBuffer (Pos bufLen) (Pos $ bufLen + 1) insertionPoint
    pieces'    = insertPiece insertionPoint newPiece pieces

insertLineImpl :: Storage 
               -> Pos 'Logical 
               -> Text
               -> IO Storage
insertLineImpl s@MkStorage{..} insertionPoint line = pure $
  s { addBuffer = addBuffer'
    , pieces    = pieces'
    }
  where
    addBuffer' = addBuffer <> line
    bufLen     = T.length addBuffer
    newPiece   = Piece AddBuffer (Pos bufLen) 
                                 (Pos $ bufLen + T.length line)
                                 insertionPoint
    pieces'    = insertPiece insertionPoint newPiece pieces

deleteImpl :: Storage 
           -> Pos 'Logical 
           -> IO Storage
deleteImpl s deletionPoint =
  deleteRangeImpl s (Range deletionPoint deletionPoint)

deleteRangeImpl :: Storage 
                -> Range 'Logical
                -> IO Storage
deleteRangeImpl s@MkStorage{..} deleteRange = pure $
  s { pieces = deletePiece deleteRange pieces }

itemAtImpl :: Storage 
           -> Pos 'Logical
           -> IO (Maybe Char)
itemAtImpl s lookupPoint = do
  res <- itemsAtImpl s (Range lookupPoint lookupPoint)
  pure $ if T.length res >= 1 then Just (T.head res) else Nothing

itemsAtImpl :: Storage 
            -> Range 'Logical
            -> IO Text
itemsAtImpl storage@MkStorage{..} r@Range{..} =
  case searchPiece rStart pieces of
    Position _ focus rs -> do
      let focus' = case splitPiece rStart focus of 
            Before x -> x
            After  x -> x
            InBetween _ x -> x
      go mempty (rangeLength r) focus' rs
  where
    go :: Text 
       -> Int 
       -> Piece 
       -> FingerTree (Pos 'Logical) Piece 
       -> IO Text
    go !acc !toRead p ps
      | toRead <= pieceLength p = do
          txt <- T.take toRead <$> pieceToStr storage p
          pure $ acc <> txt    
      | otherwise = do
          txt <- T.take toRead <$> pieceToStr storage p
          case viewl ps of
            EmptyL -> pure mempty -- not enough bytes
            (x :< xs) -> go (acc <> txt) (toRead - (pieceLength p)) x xs

{-----------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

instance Measured (Pos 'Logical) Piece where
    measure = rootDistance

-- | Converts a 'Piece' into a 'Text'.
-- FIXME(adn) This is inefficient as the piece can be huge. It shouldn't
-- really exist.
pieceToStr :: Storage -> Piece -> IO Text
pieceToStr MkStorage{..} p@Piece{..} = do
  case source of
    Original -> do
        bs <- BS.unsafePackCStringFinalizer originalBufferPtr (pieceLength p) (pure ())
        pure $ TE.decodeUtf8 bs
    AddBuffer -> pure $ addBuffer & T.drop (coerce startPos)
                                  & T.take (coerce $ endPos - startPos)

insertPiece :: Pos 'Logical 
            -> Piece 
            -> FingerTree (Pos 'Logical) Piece 
            -> FingerTree (Pos 'Logical) Piece
insertPiece logicalPos newPiece pcs = 
  case searchPiece logicalPos pcs of
    Position left focus right ->
      case splitPiece logicalPos focus of
          Before sibling ->
            left <> FT.singleton newPiece 
                 <> FT.fmap' increaseDistance (sibling <| right)
          After sibling ->
            left <> FT.fromList [sibling,newPiece] 
                 <> FT.fmap' increaseDistance right
          InBetween this that ->
            left <> FT.fromList [that, newPiece] 
                 <> FT.fmap' increaseDistance (this <| right)
    OnLeft  -> newPiece <| pcs
    OnRight -> pcs |> newPiece
    Nowhere -> pcs
  where
    increaseDistance :: Piece -> Piece
    increaseDistance = increaseRootDistance (pieceLength newPiece)

searchPiece :: Pos 'Logical 
            -> FingerTree (Pos 'Logical) Piece 
            -> SearchResult (Pos 'Logical) Piece
searchPiece pos = FT.search (\r1 r2 -> r1 <= pos && r2 > pos)

deletePiece :: Range 'Logical 
            -> FingerTree (Pos 'Logical) Piece 
            -> FingerTree (Pos 'Logical) Piece 
deletePiece deleteRange@Range{..} pcs =
  case searchPiece rStart pcs of
    Position left focus right -> error "todo"
    OnLeft  -> error "todo"
    OnRight -> error "todo"
    Nowhere -> pcs
--  z  <- Z.zipper pcs
--  z' <- flip Z.runListZipperOp z $ do
--          Z.moveRightUntil (inRange rStart)
--          Z.opUntil Z.deleteStepRight (inRange rEnd)
--  case z' of
--    (Z.ListZipper left focus right, ()) -> do
--      -- Here we have to handle the case where the deletion happens
--      -- within the same piece or between pieces.
--      if | inRange rStart focus ->
--          pure $ case splitPiece rStart focus of
--            Before sibling ->
--              left <> map decreaseDistance (sibling : right)
--            After sibling ->
--              left <> [amend sibling] <> map decreaseDistance right
--            InBetween this that ->
--              left <> map decreaseDistance (amend that : this : right)
--         | otherwise ->
--          pure $ case splitPiece rEnd focus of
--            Before sibling ->
--              left <> map decreaseDistance (sibling : right)
--            After sibling ->
--              left <> [sibling] <> map decreaseDistance right
--            InBetween this that ->
--              left <> map decreaseDistance (this : right)
  where
    decreaseDistance :: Piece -> Piece
    decreaseDistance = decreaseRootDistance (rangeLength deleteRange)

    amend :: Piece -> Piece
    amend p = p { startPos = startPos p + Pos (rangeLength deleteRange) }

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

ioEditorAPI :: TextEditorAPI PieceTable Text IO
ioEditorAPI = TextEditorAPI {
  load = \fp -> do
    bracket (acquire fp) dispose $ \(ptr, rs, offset, sz) -> do
      let initialStorage = MkStorage {
            originalBufferPtr     = ptr
          , originalBufferRawSize = rs
          , originalBufferOffset  = offset
          , originalBufferSize    = sz
          , addBuffer             = mempty
          , pieces                = mempty
          }
      pure $ fix mkIoEditor initialStorage
  }
  where
    acquire :: FilePath -> IO (Ptr Word8, Int, Int, Int)
    acquire fp = MMap.mmapFilePtr fp MMap.ReadOnly Nothing

    dispose :: (Ptr Word8, Int, Int, Int) -> IO ()
    dispose (ptr, rs, _,_) = MMap.munmapFilePtr ptr rs

