module Development.Shake.Plus.Extended.Local where

import Development.Shake.Plus
import RIO
import Path

class HasLocalOut r where
  localOut :: Lens' r (Path b Dir)

delocaliseOut :: (MonadReader r m, HasLocalOut r) => Path Rel t -> m (Path Rel t)
delocaliseOut x = ask >>= \r -> return $ (view localOut r </> x)

(/%>) :: (MonadReader r m, HasLocalOut r, MonadRules m) => FilePattern -> (Path Rel File -> RAction r ()) -> m ()
(/%>) xs ract = ask >>= \r -> do
  let d = view localOut r
  (toFilePath d <> xs) %> (ract <=< stripProperPrefix d)

(/|%>) :: (MonadReader r m, HasLocalOut r, MonadRules m) => [FilePattern] -> (Path Rel File -> RAction r ()) -> m ()
(/|%>) xs ract = ask >>= \r -> do
  let d = view localOut r
  ((toFilePath d <>) <$> xs) |%> (ract <=< stripProperPrefix d)

(/%^>) :: (MonadReader r m, HasLocalOut r, MonadRules m) => FilePattern -> (Within Rel File -> RAction r ()) -> m ()
(/%^>) xs ract = ask >>= \r -> do
  let d = view localOut r
  (toFilePath d <> xs) %> (ract <=< `asWithin` d)

(/|%^>) :: (MonadReader r m, HasLocalOut r, MonadRules m) => [FilePattern] -> (Within Rel File -> RAction r ()) -> m ()
(/|%^>) xs ract = ask >>= \r -> do
  let d = view localOut r
  ((toFilePath d <>) <$> xs) |%> (ract <=< `asWithin` d)
