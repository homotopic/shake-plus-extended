{- |
   Module     : Development.Shake.Plus.Extended.FileRules
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

-}
module Development.Shake.Plus.Extended.FileRules (
  (%^>)
, (|%^>)
) where

import           Control.Comonad.Env         as E
import           Control.Exception.Extra
import           Development.Shake.FilePath
import           Development.Shake.Plus
import           RIO                         as R
import           Within

-- | `Within` variant of `(%>)`, used to keep track of local directories.
(%^>) :: (Partial, MonadReader r m, MonadRules m) => Within Rel FilePattern -> (Within Rel (Path Rel File) -> RAction r ()) -> m ()
(%^>) xs ract = liftA2 (Development.Shake.FilePath.</>) (toFilePath . E.ask) extract xs %> (ract <=< (`asWithin` E.ask xs))

-- | `Within` variant of `(%>)`, used to keep track of local directories.
(|%^>) :: (Partial, MonadReader r m, MonadRules m) => Within Rel [FilePattern] -> (Within Rel (Path Rel File) -> RAction r ()) -> m ()
(|%^>) xs ract = ((Development.Shake.FilePath.</>) (toFilePath . E.ask $ xs) <$> extract xs) |%> (ract <=< (`asWithin` E.ask xs))
