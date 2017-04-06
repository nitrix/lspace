module Relational
    ( runRelational
    , createRelation
    , readRelation
    , updateRelation
    , viaDisk
    , viaMemory
    )
    where

import Relational.Core
import Relational.Store
import Relational.Store.Disk
import Relational.Store.Memory