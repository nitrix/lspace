module Types.Relation where

import Control.Monad.Trans
import Data.Aeson as J
import Data.IORef
import System.Mem.Weak
import System.IO.Unsafe

data Relation a = MkRelation {-# UNPACK #-} !(IORef (Int, Maybe (Weak (IORef a))))

newtype Relational m a = Relational { runRelational :: m (IO a) }

instance MonadTrans Relational where
    -- lift :: Monad m => m a -> t m a
    lift ma = Relational $ return <$> ma

instance FromJSON (Relation a) where
    parseJSON (J.Number n) = do
        return $ MkRelation $ unsafePerformIO $ newIORef $ (truncate n, Nothing)
    parseJSON _ = error "Unable to parse Relation json"

instance ToJSON (Relation a) where
    toJSON (MkRelation lnk) = Number $ fromIntegral $ fst $ unsafePerformIO $ readIORef lnk
