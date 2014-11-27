module QFeldspar.NameMonad where

import Prelude
import Control.Monad.State hiding (mapM,sequence)

type NamM m a = StateT Int m  a

newVar :: Monad m => NamM m String
newVar = do i <- get
            put (i + 1)
            return ("v" ++ show i)

runNamM :: Monad m => NamM m a -> m a
runNamM = flip evalStateT 0
