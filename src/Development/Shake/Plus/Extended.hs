{- |
   Module     : Development.Shake.Plus.Extended
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech>
   License    : MIT
   Stability  : experimental

Module exports for Development.Shake.Plus.Extended.
-}

module Development.Shake.Plus.Extended (
  module Development.Shake.Plus.Extended.FileRules
, module Development.Shake.Plus.Extended.Loaders
, module Development.Shake.Plus.Extended.Simple
) where

import Development.Shake.Plus.Extended.FileRules
import Development.Shake.Plus.Extended.Loaders
import Development.Shake.Plus.Extended.Simple
import Path.Binary()
import Data.IxSet.Typed.Binary()
import Data.IxSet.Typed.Hashable()
