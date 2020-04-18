{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Text.Editor.PieceTable.Types (
      Piece(..)
    , Source(..)
    , renderPiece
    , pieceFromStr
    , pieceLength
    , increaseRootDistance
    , decreaseRootDistance
    , inRange
    , SplitResult(..)
    , splitPiece
    , isNull
    ) where

import Text.Editor.Types
import Text.Editor.Editable as E

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

data Source =
    Original
  | AddBuffer
  deriving (Eq, Show)

-- | A 'Piece'.
data Piece = Piece {
    source       :: Source
  , startPos     :: Pos 'Physical
  , endPos       :: Pos 'Physical
  , rootDistance :: Pos 'Logical
  -- ^ The \"distance\" (expressed as a /logical/ position) from the
  -- root of the document.
  } deriving (Eq, Show)

renderPiece :: Piece -> String
renderPiece p@Piece{..} =
    let sourceTxt = case source of
          Original  -> "O"
          AddBuffer -> "A"
    in "Piece<" <> sourceTxt <> "," <> show (pieceLength p) <> ">" 
                <> show (getPos startPos) <> ":" <> show (getPos endPos)
                <> ":R=" <> show (getPos rootDistance)

-- | Builds a new 'Piece' out of an input 'Editable' string.
pieceFromStr :: Editable str => str -> Source -> Piece
pieceFromStr str s = 
    Piece s (Pos 0) (Pos $ Editable.length str) (Pos 0)

pieceLength :: Piece -> Int
pieceLength Piece{..} = coerce (endPos - startPos)

increaseRootDistance :: Int -> Piece -> Piece
increaseRootDistance distance p = 
  p { rootDistance = rootDistance p + Pos distance }

decreaseRootDistance :: Int -> Piece -> Piece
decreaseRootDistance distance p = 
  p { rootDistance = rootDistance p - Pos distance }

-- | Returns 'True' if the input 'Pos' falls within the input 'Piece'.
inRange :: Pos 'Logical -> Piece -> Bool
inRange pos p = pos >= rootDistance p && 
                pos <= rootDistance p + Pos (pieceLength p)

data SplitResult =
    InBetween Piece Piece
  | Before Piece
  | After Piece
  deriving (Eq, Show)

splitPiece :: Pos 'Logical -> Piece -> SplitResult
splitPiece pos piece@Piece{..}
  | pos <=  rootDistance                            = Before piece
  | pos >= (rootDistance + Pos (pieceLength piece)) = After piece
  | otherwise =
      let slack = pos - rootDistance
          left  = Piece source startPos (startPos + coerce slack) rootDistance
          right = Piece source (startPos + coerce slack) endPos rootDistance
      in InBetween left right
  where
    slack :: Pos 'Physical
    slack = coerce (pos - rootDistance)

isNull :: Piece -> Bool
isNull Piece{..} = startPos >= endPos
