{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Subtask.Internal where

import ClassyPrelude
import Control.Lens  (makeLenses, (%~))
import Data.Taskell.Repo.Internal

type Update = Subtask -> Subtask

-- operations
blank :: Subtask
blank = Subtask "" False

new :: Text -> Bool -> Subtask
new = Subtask

new' :: Text -> Bool -> Taskell 'SubtaskType
new' t = SubtaskE . Identity . new t

toggle :: Update
toggle = complete %~ not
