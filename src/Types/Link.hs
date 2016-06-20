module Types.Link where

import Data.IORef
import System.Mem.Weak

data Link a = LinkId Int | LinkRef Int (Weak (IORef a))
