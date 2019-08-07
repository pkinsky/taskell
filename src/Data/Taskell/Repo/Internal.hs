{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Taskell.Repo.Internal where
  -- ( TaskellKind (..)
  -- , Taskell
  -- , Taskell'(..)
  -- , description
  -- , due
  -- )where

import ClassyPrelude

import Control.Lens hiding (List', List)

import Data.Sequence as S (adjust', deleteAt, insertAt, update, (|>))

import qualified Data.Taskell.Seq  as S


data TaskellKind = RepoType | ListType | TaskType | SubtaskType


-- | 'f' represents indirection - could be Const Hashpointer, 'm' for some monad, etc
data Taskell' a (i :: TaskellKind) where
  RepoE    :: Repo' a -> Taskell' a 'RepoType -- can later add arbitrary top-level info/etc here
  ListE    :: List' a -> Taskell' a 'ListType
  TaskE    :: Task' a -> Taskell' a 'TaskType
  -- TODO: add option here, specifically:
  -- TODO:  - inline images! can use some img -> ascii to render, or open in external
  -- TODO:  - opens up some awesome options
  -- NOTE: could have file trees inline -- probably just as hash pointers
  SubtaskE :: Subtask -> Taskell' a 'SubtaskType

type Taskell = Taskell' Identity
deriving instance Eq (Taskell' Identity i)
deriving instance Show (Taskell' Identity i)

-- type Hash = Int -- todo idk cryptonite or w/e
-- type WithHashPointer = (,) Hash
-- type JustHashPointer = Const Hash

-- addHashesR
--   :: Taskell' Identity 'RepoType
--   -> Taskell' WithHashPointer 'RepoType
-- addHashesR (RepoE (Identity r)) = RepoE (hash, r')
--   where r' = RepoE $ fmap addHashesL r
--         hash = hashRepo r

-- hashRepo :: Repo' JustHashPointer -> Hash
-- hashRepo = undefined


-- addHashesL
--   :: Taskell' Identity 'ListType
--   -> Taskell' WithHashPointer 'ListType
-- addHashesL (ListE (Identity l)) = undefined


-- Lenses for GADT

tRepo :: Lens' (Taskell 'RepoType) Repo
tRepo = lens (\(RepoE (Identity r)) -> r) (\_ r' -> RepoE $ Identity r')

tSubtask :: Lens' (Taskell 'SubtaskType) Subtask
tSubtask = lens (\(SubtaskE (Identity s)) -> s) (\_ s' -> SubtaskE $ Identity s')

data Repo' f = Repo
    { _repo'Lists :: Seq (Taskell' f 'ListType)
    }

type Repo = Repo' Identity
deriving instance Eq   Repo
deriving instance Show Repo


data List' f = List
    { _list'Title :: Text
    , _list'Tasks :: Seq (Taskell' f 'TaskType)
    }

type List = List' Identity
deriving instance Eq   List
deriving instance Show List

data Task' f = Task
    { _task'Name        :: Text
    , _task'Description :: Maybe Text
    , _task'Subtasks    :: Seq (Taskell' f 'SubtaskType)
    , _task'Due         :: Maybe Day
    }

type Task = Task' Identity
deriving instance Eq   Task
deriving instance Show Task

data Subtask = Subtask
    { _subtaskName     :: Text
    , _subtaskComplete :: Bool
    } deriving (Eq, Show)


-- create lenses
$(makeFields ''Repo')
$(makeFields ''List')
$(makeFields ''Task')
$(makeFields ''Subtask)
