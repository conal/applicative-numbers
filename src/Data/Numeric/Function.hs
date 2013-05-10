{-# LANGUAGE FlexibleContexts, CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Numeric.Function
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Numeric instances for functions
----------------------------------------------------------------------

module Data.Numeric.Function () where

import Text.Show.Functions ()
import Control.Applicative (Applicative(..),liftA2)

{--------------------------------------------------------------------
    Numeric instances for functions, via the applicative-numbers package
--------------------------------------------------------------------}

-- Generate bogus (error-producing) Eq, Ord & Enum, but not Show, which
-- comes from Text.Show.Functions
#define INSTANCE_Eq
#define INSTANCE_Ord
#define INSTANCE_Enum

-- #define APPLICATIVE ((->) a)
-- Avoid GHC 7.6.3 bug (?) by stripping the parens. Luckily, they're not needed here.
-- The Eq instance for (((->) a) b) is not recognized as equivalent to (a -> b).

#define APPLICATIVE (->) a
#include "ApplicativeNumeric-inc.hs"
