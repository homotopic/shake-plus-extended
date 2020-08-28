{- |
   Module     : Development.Shake.Plus.Extended.Simple
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech>
   License    : MIT
   Stability  : experimental

Shortcuts to run shake-plus with a simple environment.
-}
module Development.Shake.Plus.Extended.Simple (
  SimpleSPlusEnv(..)
, runLoggedShakePlus
, runSimpleShakePlus
) where

import           Development.Shake.Plus
import           Development.Shake.Plus.Extended.FileRules
import           RIO

data SimpleSPlusEnv = SimpleSPlusEnv {
  logFunc  :: LogFunc
, localOut :: Path Rel Dir
}

instance HasLogFunc SimpleSPlusEnv where
  logFuncL = lens logFunc (\x y -> x { logFunc = y} )

instance HasLocalOut SimpleSPlusEnv where
  localOutL = lens localOut (\x y -> x { localOut = y})

-- | Run a `ShakePlus` with just a `LogFunc` in the environment that logs to stderr.
runLoggedShakePlus :: MonadIO m => ShakePlus LogFunc a -> m ()
runLoggedShakePlus m = do
  lo <- logOptionsHandle stderr True
  (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo lo)
  liftIO $ shakeArgs shakeOptions $ void $ runShakePlus lf m
  dlf


-- | Run a `ShakePlus` with just a `SimpleSPlusEnv`.
runSimpleShakePlus :: MonadIO m => Path Rel Dir -> ShakePlus SimpleSPlusEnv a -> m ()
runSimpleShakePlus outputFolder m = do
  lo <- logOptionsHandle stderr True
  (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo lo)
  let env = SimpleSPlusEnv lf outputFolder
  liftIO $ shakeArgs shakeOptions $ void $ runShakePlus env m
  dlf
