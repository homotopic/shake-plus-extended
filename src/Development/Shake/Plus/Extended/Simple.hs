{- |
   Module     : Development.Shake.Plus.Extended.Simple
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech>
   License    : MIT
   Stability  : experimental

Shortcuts to run shake-plus with a simple environment.
-}
module Development.Shake.Plus.Extended.Simple (
 runSimpleShakePlus
) where

import           Control.Exception
import           Development.Shake.Plus
import           RIO

-- | Run a `ShakePlus` with just a `LogFunc` in the environment that logs to stderr.
runSimpleShakePlus :: MonadIO m => ShakePlus LogFunc a -> m ()
runSimpleShakePlus m = do
  lo <- logOptionsHandle stderr True
  (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo lo)
  liftIO $ shakeArgs shakeOptions $ void $ runShakePlus lf m
  dlf
