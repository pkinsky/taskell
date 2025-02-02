{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Events.State.Types where

import ClassyPrelude

import Control.Lens (makeLenses)

import Data.Taskell.Lists (Lists)
import UI.Field           (Field)

import qualified Events.State.Types.Mode as M (Mode)

type Pointer = (Int, Int)

data State = State
    { _mode       :: M.Mode
    , _lists      :: Lists
    , _history    :: [(Pointer, Lists)]
    , _current    :: Pointer
    , _path       :: FilePath
    , _io         :: Maybe Lists
    , _height     :: Int
    , _searchTerm :: Maybe Field
    } deriving (Eq, Show)

-- create lenses
$(makeLenses ''State)

type Stateful = State -> Maybe State
