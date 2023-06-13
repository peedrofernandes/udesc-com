module Handlers where

import Control.Monad.State
import Control.Monad.IO.Class

warn :: MonadIO m => String -> m ()
warn str = liftIO $ putStrLn $ "Warning: " ++ str
