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
data Taskell' f (i :: TaskellKind) where
  RepoE    :: f (Repo' f) -> Taskell' f 'RepoType -- can later add arbitrary top-level info/etc here
  ListE    :: f (List' f) -> Taskell' f 'ListType
  TaskE    :: f (Task' f) -> Taskell' f 'TaskType
  -- TODO: add option here, specifically:
  -- TODO:  - inline images! can use some img -> ascii to render, or open in external
  -- TODO:  - opens up some awesome options
  SubtaskE :: f Subtask   -> Taskell' f 'SubtaskType

type Taskell = Taskell' Identity
deriving instance Eq (Taskell' Identity i)
deriving instance Show (Taskell' Identity i)


-- addHashes
--   :: (forall i. Taskell' Identity i -> hash)
--   -> Taskell' Identity i
--   -> Taskell' ((,) hash) i
-- addHashes

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
