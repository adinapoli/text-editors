{-# LANGUAGE EmptyDataDecls #-}
module Text.Editor.Reference where

import Control.Monad.Identity
import Text.Editor.Types

data Reference str
type instance InternalStorage (Reference str) = [str]

referenceStorage :: InternalStorage Reference
referenceStorage = []

type ReferenceEditor str m = TextEditor Reference str m

pureEditor :: ReferenceEditor str Identity
pureEditor = TextEditor
    { _storage   = referenceStorage
    , insert     = insertImpl     pureEditor 
    , insertLine = insertLineImpl pureEditor
    , delete     = deleteImpl     pureEditor
    , deleteLine = deleteLineImpl pureEditor
    }

insertImpl :: ReferenceEditor str Identity
           -> Pos 
           -> Rune str 
           -> ReferenceEditor str Identity
insertImpl old pos rune =
    old { _storage = insertChar (_storage old) }
  where
    insertChar = []

insertLineImpl :: ReferenceEditor str Identity
               -> Pos 
               -> str 
               -> ReferenceEditor str Identity
insertLineImpl old pos rune =
    old { _storage = insertStr (_storage old) }
  where
    insertStr = []

