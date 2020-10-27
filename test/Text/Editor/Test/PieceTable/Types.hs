{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Text.Editor.Test.PieceTable.Types where

import Control.Monad.Identity
import Data.Proxy
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Text.Editor.PieceTable.Types
import Text.Editor.Types
import Text.Editor.Test

tests :: TestTree
tests = testGroup "PieceTable.Types tests" [
      testProperty "inRange_1" inRange_1_prop
    , testProperty "inRange_2" inRange_2_prop
    , testProperty "inRange_3" inRange_3_prop
    , testProperty "splitPiece_1" splitPiece_1_prop
    , testProperty "splitPiece_2" splitPiece_2_prop
    , testProperty "splitPiece_3" splitPiece_3_prop
    ]

-- in range, position at the end of the piece.
inRange_1_prop :: Property
inRange_1_prop =
  inRange (Pos 10) (Piece Original (Pos 0) (Pos 10) (Pos 0)) === True

inRange_2_prop :: Property
inRange_2_prop =
  inRange (Pos 10) (Piece Original (Pos 0) (Pos 5) (Pos 0)) === False

-- in range, position at the beginning of the piece.
inRange_3_prop :: Property
inRange_3_prop =
  inRange (Pos 0) (Piece Original (Pos 4) (Pos 9) (Pos 0)) === True

-- Split at piece boundary, end of piece.
splitPiece_1_prop :: Property
splitPiece_1_prop =
  splitPiece (Pos 10) (Piece Original (Pos 0) (Pos 10) (Pos 0)) ===
      (After (Piece Original (Pos 0) (Pos 10) (Pos 0)))

-- Split at piece boundary, end of piece.
splitPiece_2_prop :: Property
splitPiece_2_prop =
  splitPiece (Pos 0) (Piece Original (Pos 0) (Pos 10) (Pos 0)) ===
      (Before (Piece Original (Pos 0) (Pos 10) (Pos 0)))

-- Split in the middle.
splitPiece_3_prop :: Property
splitPiece_3_prop =
  splitPiece (Pos 5) (Piece Original (Pos 9) (Pos 18) (Pos 0)) ===
      (InBetween (Piece Original (Pos 9) (Pos 14) (Pos 0))
                 (Piece Original (Pos 14) (Pos 18)  (Pos 0))
      )
