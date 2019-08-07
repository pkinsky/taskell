{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Task.Internal where

import ClassyPrelude

import Control.Lens (ix, makeLenses, (%~), (&), (.~), (^.), (^?), over)

import           Data.Sequence        as S (adjust', deleteAt, (|>))
import           Data.Taskell.Date    (Day, textToDay)
import           Data.Taskell.Repo.Internal
import qualified Data.Taskell.Subtask as ST
import           Data.Text            (strip)

type Update = Task -> Task


-- operations
blank :: Task
blank = Task "" Nothing empty Nothing

new :: Text -> Task
new text = blank & (name .~ text)

setDescription :: Text -> Update
setDescription text =
    description .~
    if null (strip text)
        then Nothing
        else Just text

maybeAppend :: Text -> Maybe Text -> Maybe Text
maybeAppend text (Just current) = Just (concat [current, "\n", text])
maybeAppend text Nothing        = Just text

appendDescription :: Text -> Update
appendDescription text =
    if null (strip text)
        then id
        else description %~ maybeAppend text

setDue :: Text -> Update
setDue date task =
    if null date
        then task & due .~ Nothing
        else case textToDay date of
                 Just day -> task & due .~ Just day
                 Nothing  -> task

getSubtask :: Int -> Task -> Maybe ST.Subtask
getSubtask idx = (^? subtasks . ix idx . tSubtask)

addSubtask :: ST.Subtask -> Update
addSubtask subtask = subtasks %~ (|> SubtaskE (Identity subtask))

hasSubtasks :: Task -> Bool
hasSubtasks = not . null . (^. subtasks)

updateSubtask :: Int -> ST.Update -> Update
updateSubtask idx fn = subtasks %~ adjust' (tSubtask %~ fn) idx

removeSubtask :: Int -> Update
removeSubtask idx = subtasks %~ S.deleteAt idx

countSubtasks :: Task -> Int
countSubtasks = length . (^. subtasks)

countCompleteSubtasks :: Task -> Int
countCompleteSubtasks = length . filter (^. tSubtask . ST.complete) . (^. subtasks)

contains :: Text -> Task -> Bool
contains text task =
    check (task ^. name) || maybe False check (task ^. description) || not (null sts)
  where
    check = isInfixOf (toLower text) . toLower
    sts = filter check $ (^. tSubtask . ST.name) <$> (task ^. subtasks)

isBlank :: Task -> Bool
isBlank task =
    null (task ^. name) &&
    isNothing (task ^. description) && null (task ^. subtasks) && isNothing (task ^. due)
