{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Editor.PieceTable.IO
import Text.Editor.Types

main :: IO ()
main = do
  load "data-files/russian_dict.txt" ioEditorAPI $ \editor -> do
    -- editor <- load "data-files/scratchpad.txt" api
    itemsAt (Range (Pos 10) (Pos 20)) editor >>= print
    itemsAt (Range (Pos 10000) (Pos 10010)) editor >>= print
