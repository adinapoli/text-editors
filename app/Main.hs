{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Text.Editor.PieceTable.IO as PT
import qualified Text.Editor.Reference.IO  as Ref
import Text.Editor.Types

main :: IO ()
main = do
  load "data-files/russian_dict.txt" PT.ioEditorAPI $ \editor -> do
    itemsAt (Range (Pos 10) (Pos 20)) editor >>= print
    itemsAt (Range (Pos 10000) (Pos 10010)) editor >>= print
  load "data-files/scratchpad.txt" Ref.ioEditorAPI $ \editor -> do
    itemsAt (Range (Pos 0) (Pos 12)) editor >>= print
