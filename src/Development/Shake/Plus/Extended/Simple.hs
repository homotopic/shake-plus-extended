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
, runLoggedShakeForward
) where

import Development.Shake.Plus
import Development.Shake.Plus.Forward
import Development.Shake.Plus.Extended.FileRules
import RIO

data SimpleSPlusEnv = SimpleSPlusEnv {
  logFunc  :: LogFunc
, localOut :: Path Rel Dir
}

instance HasLogFunc SimpleSPlusEnv where
  logFuncL = lens logFunc (\x y -> x { logFunc = y} )

instance HasLocalOut SimpleSPlusEnv where
  localOutL = lens localOut (\x y -> x { localOut = y})

-- | Run a `ShakePlus` with just a `LogFunc` in the environment that logs to stderr.
runLoggedShakePlus :: MonadIO m => ShakeOptions -> ShakePlus LogFunc a -> m ()
runLoggedShakePlus opts m = do
  lo <- logOptionsHandle stderr True
  (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo lo)
  liftIO $ shakeArgs opts $ void $ runShakePlus lf m
  dlf

-- | Run a `ShakePlus` with just a `SimpleSPlusEnv`.
runSimpleShakePlus :: MonadIO m => Path Rel Dir -> ShakeOptions -> ShakePlus SimpleSPlusEnv a -> m ()
runSimpleShakePlus outputFolder opts m = do
  lo <- logOptionsHandle stderr True
  (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo lo)
  let env = SimpleSPlusEnv lf outputFolder
  liftIO $ shakeArgs opts $ void $ runShakePlus env m
  dlf

-- | Run an `RAction` in forward mode.
runLoggedShakeForward :: MonadIO m => ShakeOptions -> RAction LogFunc () -> m ()
runLoggedShakeForward opts m = do
  lo <- logOptionsHandle stderr True
  (lf, dlf) <- newLogFunc (setLogMinLevel LevelInfo lo)
  shakeArgsForward opts lf m
  dlf
