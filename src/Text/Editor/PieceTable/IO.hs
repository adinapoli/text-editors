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
    -- * Debug utility functions
    , debugDumpStorage
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
import Data.Foldable (toList, foldlM)
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
import qualified Data.Text.IO as TIO

import Foreign.Ptr
import System.IO
import System.IO.MMap as MMap
import System.IO.Posix.MMap.Internal

import Debug.Trace

-- A type tag.
data PieceTable

type instance InternalStorage PieceTable = Storage

data Storage = MkStorage {
    originalBufferPtr     :: Ptr Word8
  , originalBufferRawSize :: !Int
  , originalBufferOffset  :: !Int
  , originalBufferSize    :: !Int
  , addBuffer             :: Text
  , pieces                :: FingerTree PieceMeasure Piece
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
itemsAtImpl storage@MkStorage{..} r@Range{..} =  do

  -- General idea: \"chop\" the pieces to include only the range we
  -- care about, and then read each of them.

  let interestingPieces = 
        pieces & FT.dropUntil (inRange' rStart)
               & FT.takeUntil (not . (inRange' rEnd))

  case viewl interestingPieces of
    EmptyL             -> pure mempty
    (x :< xs)          -> case splitPiece rStart x of
      Before _         -> processRest (x <| xs)
      After  _         -> processRest (x <| xs)
      InBetween _ that -> processRest (that <| xs)

  where
    processRest :: FingerTree PieceMeasure Piece -> IO Text
    processRest pcs = case viewr pcs of
      EmptyR             -> pure mempty
      (left :> focus)    -> case splitPiece (rEnd - rStart + 1) focus of
        Before _         -> readRange (left |> focus)
        After  _         -> readRange (left |> focus)
        InBetween this _ -> readRange (left |> this)

    readRange :: FingerTree PieceMeasure Piece -> IO Text
    readRange = 
      foldlM (\acc p -> (`mappend` acc) <$> pieceToStr storage p) mempty


{-----------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

newtype PieceMeasure = PieceMeasure (Pos 'Logical, Int)

instance Semigroup PieceMeasure where
  PieceMeasure (p1,l1) <> PieceMeasure (p2, l2)
    = PieceMeasure (p1,l1)

instance Monoid PieceMeasure where
    mempty  = PieceMeasure (Pos 0, 0)
    mappend = (<>)

instance Measured PieceMeasure Piece where
    measure p = PieceMeasure (rootDistance p, pieceLength p)

-- | Retrieves the /entire/ content of the internal storage by reconstructing
-- the content using the 'Piece's.
debugDumpStorage :: Editor -> IO Text
debugDumpStorage ts = foldlM f mempty (toList $ pieces (_storage ts))
  where
    f :: Text -> Piece -> IO Text
    f acc p = do
      txt <- pieceToStr (_storage ts) p
      pure $ acc <> txt

-- | Reads an /entire/ 'Piece', loading its content into memory. This
-- function needs to be used with care, as it might be very inefficient
-- code if called on big pieces.
pieceToStr :: Storage -> Piece -> IO Text
pieceToStr MkStorage{..} p@Piece{..} = do
  case source of
    Original | originalBufferRawSize == 0 -> pure mempty
    Original -> do
        bs <- unsafePackMMapPtr
                 (originalBufferPtr 
                   `plusPtr` originalBufferOffset 
                   `plusPtr` coerce startPos)
                 (fromIntegral $ pieceLength p) 
        pure $ TE.decodeUtf8 bs
    AddBuffer -> pure $ addBuffer & T.drop (coerce startPos)
                                  & T.take (coerce $ endPos - startPos)

insertPiece :: Pos 'Logical 
            -> Piece 
            -> FingerTree PieceMeasure Piece 
            -> FingerTree PieceMeasure Piece
insertPiece logicalPos newPiece pcs = 
  case searchPiece logicalPos pcs of
    Position left focus right -> insert left focus right
    OnLeft -> case viewl pcs of
      EmptyL -> FT.singleton newPiece
      (focus :< right) -> insert mempty focus right
    OnRight -> case viewr pcs of
      EmptyR -> FT.singleton newPiece
      (left :> focus) -> insert left focus mempty
    Nowhere -> pcs
  where

    insert :: FingerTree PieceMeasure Piece
           -> Piece
           -> FingerTree PieceMeasure Piece
           -> FingerTree PieceMeasure Piece
    insert left focus right =
      case splitPiece logicalPos focus of
          Before sibling ->
            left <> FT.singleton newPiece 
                 <> FT.fmap' increaseDistance (sibling <| right)
          After sibling ->
            left <> FT.fromList [sibling,newPiece] 
                 <> FT.fmap' increaseDistance right
          InBetween this that ->
            left <> FT.fromList [this,newPiece] 
                 <> FT.fmap' increaseDistance (that <| right)

    increaseDistance :: Piece -> Piece
    increaseDistance = 
      increaseRootDistance (getPos (rootDistance newPiece) + pieceLength newPiece)

searchPiece :: Pos 'Logical 
            -> FingerTree PieceMeasure Piece 
            -> SearchResult PieceMeasure Piece
searchPiece pos = FT.search (\m1 _ -> inRange' pos m1)

-- | A version of 'inRange' that works over a 'PieceMeasure'.
inRange' :: Pos 'Logical -> PieceMeasure -> Bool
inRange' pos (PieceMeasure (rd, pl)) = pos >= rd && pos <= rd + Pos pl

deletePiece :: Range 'Logical 
            -> FingerTree PieceMeasure Piece 
            -> FingerTree PieceMeasure Piece 
deletePiece deleteRange@Range{..} pcs =
  case searchPiece rStart pcs of
    Position left focus right -> delete left focus right
    OnLeft -> case viewl pcs of
      EmptyL -> pcs
      (focus :< right) -> delete mempty focus right
    OnRight -> case viewr pcs of
      EmptyR -> pcs
      (left :> focus) -> delete left focus mempty
    Nowhere -> pcs
  where
    decreaseDistance :: Piece -> Piece
    decreaseDistance = decreaseRootDistance (rangeLength deleteRange)

    amend :: Piece -> Piece
    amend p = p { startPos = startPos p + Pos (rangeLength deleteRange) }

    amendFromEnd :: Piece -> Piece
    amendFromEnd p = p { startPos = endPos p - Pos (rangeLength deleteRange) }

    delete :: FingerTree PieceMeasure Piece
           -> Piece
           -> FingerTree PieceMeasure Piece
           -> FingerTree PieceMeasure Piece
    delete left focus right =
      -- Here we have to handle the case where the deletion happens
      -- within the same piece or between pieces.
      if | inRange rStart focus ->
          case splitPiece rStart focus of
            Before sibling ->
              left <> fmap' decreaseDistance (amend sibling <| right)
            After sibling ->
              left <> FT.singleton (amendFromEnd sibling) 
                   <> fmap' decreaseDistance right
            InBetween this that ->
              left <> FT.singleton this 
                   <> fmap' decreaseDistance (amend that <| right)
         | otherwise -> do
          case splitPiece rEnd focus of
            Before sibling ->
              left <> fmap' decreaseDistance (amend sibling <| right)
            After sibling ->
              left <> FT.singleton (amendFromEnd sibling) 
                   <> fmap' decreaseDistance right
            InBetween this that ->
              left <> fmap' decreaseDistance (that <| right)

{-----------------------------------------------------------------------------
  Concrete Implementations
------------------------------------------------------------------------------}

ioEditorAPI :: TextEditorAPI PieceTable Text IO
ioEditorAPI = TextEditorAPI {
    _load = \fp act -> do
      (ptr, rs, offset, sz) <- acquire fp
      let newPiece = Piece Original (Pos 0) (Pos sz) (Pos 0)
      let initialStorage = MkStorage {
            originalBufferPtr     = ptr
          , originalBufferRawSize = rs
          , originalBufferOffset  = offset
          , originalBufferSize    = sz
          , addBuffer             = mempty
          , pieces                =
              if sz == 0 then mempty else FT.singleton newPiece
          }
      act (fix mkIoEditor initialStorage) `finally` dispose (ptr, rs, offset,sz)
  , _save = \fp editor -> do
      writeHdl <- openFile fp WriteMode
      let pcs = toList . pieces . _storage $ editor
      mapM_ (writePiece writeHdl (_storage editor)) pcs
  }
  where
    acquire :: FilePath -> IO (Ptr Word8, Int, Int, Int)
    acquire fp = MMap.mmapFilePtr fp MMap.ReadOnly Nothing

    dispose :: (Ptr Word8, Int, Int, Int) -> IO ()
    dispose (ptr, rs, _,_) = MMap.munmapFilePtr ptr rs

    writePiece :: Handle -> Storage -> Piece -> IO ()
    writePiece hdl sto p = 
      pieceToStr sto p >>= hPutStr hdl . T.unpack
