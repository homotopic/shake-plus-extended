{- |
   Module     : Development.Shake.Plus.Extended.FileRules
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech>
   License    : MIT
   Stability  : experimental

-}
module Development.Shake.Plus.Extended.FileRules (
  HasLocalOut(..)
, (/%>)
, (|/%>)
, (%^>)
, (|%^>)
) where

import           Control.Comonad.Env         as E
import           Control.Exception.Extra
import           Development.Shake.FilePath
import           Development.Shake.Plus
import           RIO                         as R
import           Within

class HasLocalOut r where
  localOutL :: Lens' r (Path Rel Dir)

-- | Variant of `(%>)` that passes the local directory from the environment into the callback.
(/%>) :: (MonadReader r m, HasLocalOut r, MonadRules m) => FilePattern -> ((Path Rel Dir, Path Rel File) -> RAction r ()) -> m ()
(/%>) xs ract = ask >>= \r -> do
  let d = view localOutL r
  (toFilePath d <> xs) %> (stripProperPrefix d >=> \x -> ract (d, x))

-- | Variant of `(|%>)` that passes the local directory from the environment into the callback.
(/|%>) :: (MonadReader r m, HasLocalOut r, MonadRules m) => [FilePattern] -> ((Path Rel Dir, Path Rel File) -> RAction r ()) -> m ()
(/|%>) xs ract = ask >>= \r -> do
  let d = view localOutL r
  ((toFilePath d <>) <$> xs) |%> (stripProperPrefix d >=> \x -> ract (d, x))

-- | `Within` variant of `(%>)`, used to keep track of local directories.
(%^>) :: (Partial, MonadReader r m, MonadRules m) => Within Rel FilePattern -> (Within Rel (Path Rel File) -> RAction r ()) -> m ()
(%^>) xs ract = liftA2 (Development.Shake.FilePath.</>) (toFilePath . E.ask) extract xs %> (ract <=< (`asWithin` E.ask xs))

-- | `Within` variant of `(%>)`, used to keep track of local directories.
(|%^>) :: (Partial, MonadReader r m, MonadRules m) => Within Rel [FilePattern] -> (Within Rel (Path Rel File) -> RAction r ()) -> m ()
(|%^>) xs ract = ((Development.Shake.FilePath.</>) (toFilePath . E.ask $ xs) <$> extract xs) |%> (ract <=< (`asWithin` E.ask xs))
