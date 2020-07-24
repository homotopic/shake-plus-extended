module Development.Shake.Plus.Extended.Local where

import Development.Shake.Plus
import RIO
import Path
import Within

class HasLocalOut r where
  localOutL :: Lens' r (Path Rel Dir)

(/%>) :: (MonadReader r m, HasLocalOut r, MonadRules m) => FilePattern -> ((Path Rel Dir, Path Rel File) -> RAction r ()) -> m ()
(/%>) xs ract = ask >>= \r -> do
  let d = view localOutL r
  (toFilePath d <> xs) %> (\x -> ract (d, x))

(/|%>) :: (MonadReader r m, HasLocalOut r, MonadRules m) => [FilePattern] -> ((Path Rel Dir, Path Rel File) -> RAction r ()) -> m ()
(/|%>) xs ract = ask >>= \r -> do
  let d = view localOutL r
  ((toFilePath d <>) <$> xs) |%> (\x -> ract (d, x))
