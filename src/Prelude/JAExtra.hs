{-|
Module      : $HEADER$
Description : The most useful out of the common extra functions.
Copyright   : (c) Justus Adam, 2015
License     : BDS3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX, Windows
-}
module Prelude.JAExtra
  (
  -- * Misc

    onlyIf

  -- * Tuples

  , curry2, curry3, curry4, curry5
  , uncurry2, uncurry3, uncurry4, uncurry5

  -- * Lists

  , fillZip, fillZip3
  , monoidFillZip, monoidFillZip3

  -- * Functions

  , stuffWith2, stuffWith3

  ) where


import Data.List.JAExtra
import Data.Tuple.JAExtra
import Data.Maybe.JAExtra
import Data.Function.JAExtra
