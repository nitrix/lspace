{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Relational.Types where

import Control.Monad.State
import Data.Aeson
import Data.Dynamic
import qualified Data.HashTable.IO as H
import Data.Label (mkLabel)
import qualified Data.Map as M

-- Related a :: Contraint
type Related a = (Typeable a, ToJSON a, FromJSON a)

type RelationId = Integer -- TODO: How about a base26 or more string?
data Relation a = Relation RelationId

newtype Relational s a = Relational { unwrapRelational :: StateT (Context s) IO a }
    deriving (Functor, Applicative, Monad, MonadState (Context s))

-- TODO: labels
data Context s = Context
    { _ctxCache :: H.LinearHashTable RelationId (Dynamic, StateT s IO ()) -- TODO: Should this tuple become a record?
    , _ctxStore :: s
    }
mkLabel ''Context