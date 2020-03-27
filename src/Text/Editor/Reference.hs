{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Editor.Reference where

import Text.Editor.Types
import Text.Editor.Editable as E

import Control.Monad.Identity

import Data.Map.Strict (Map)
import Data.String
import qualified Data.Map.Strict as M

data Reference str

--data instance InternalStorage (Reference str) = 
--  Storage where
--      MkStorage :: Editable str => Map Row (Map Col (Rune str))

type instance InternalStorage (Reference str) = Storage str

data Storage str where
  MkStorage :: Editable str 
            => Map Row (Map Col (Rune str))
            -> Storage str

modifyStorage :: (Map Row (Map Col (Rune str)) -> (Map Row (Map Col (Rune str))))
              -> Storage str 
              -> Storage str
modifyStorage f (MkStorage s) = MkStorage $ f s

referenceStorage :: InternalStorage (Reference str)
referenceStorage = MkStorage mempty

type ReferenceEditor str m = TextEditor (Reference str) str m

pureEditor :: ReferenceEditor str Identity
pureEditor = TextEditor
    { _storage   = referenceStorage
    , _insert     = insertImpl     pureEditor 
    , _insertLine = insertLineImpl pureEditor
    , _delete     = deleteImpl     pureEditor
    , _deleteLine = deleteLineImpl pureEditor
    }

insertImpl :: ReferenceEditor str Identity
           -> Pos 
           -> Rune str 
           -> ReferenceEditor str Identity
insertImpl old pos rune =
    old { _storage = insertChar (_storage old) }
  where
    insertChar = mempty

insertLineImpl :: ReferenceEditor str Identity
               -> Row
               -> str 
               -> ReferenceEditor str Identity
insertLineImpl old row line =
    old { _storage = modifyStorage (M.alter insertStr row) (_storage old)
        }
  where
    insertStr Nothing = Just $ M.fromList $ E.zip (fromString $ map Col [0..]) str

deleteImpl :: ReferenceEditor str Identity
           -> Pos
           -> ReferenceEditor str Identity
deleteImpl old (Pos row col) =
    old { _storage = modifyStorage (M.alter deleteStr row) (_storage old) }
  where
    deleteStr Nothing  = Nothing
    deleteStr (Just m) = Just (M.delete col m)

deleteLineImpl :: ReferenceEditor str Identity
               -> Row
               -> ReferenceEditor str Identity
deleteLineImpl old row =
    old { _storage = modifyStorage (M.delete row) (_storage old) }
