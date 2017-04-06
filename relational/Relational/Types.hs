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

data Context s = Context
    { ctxCache :: H.LinearHashTable RelationId Dynamic
    , ctxStore :: s
    }

newtype ViaDisk = ViaDisk { _diskNamespace :: String }
data ViaMemory = ViaMemory { _memoryMapping :: M.Map Integer Dynamic
                           , _memoryIncrement :: Integer
                           }

mkLabel ''ViaMemory
mkLabel ''ViaDisk