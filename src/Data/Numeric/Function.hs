{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, CPP #-}
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
-- Vector types
----------------------------------------------------------------------

module Data.Numeric.Function () where

import Text.Show.Functions ()
import Control.Applicative (Applicative(..),liftA2)

{--------------------------------------------------------------------
    Numeric instances, via the applicative-numbers package
--------------------------------------------------------------------}

-- Generate Eq, Ord & Enum, but not Show
#define INSTANCE_Eq
#define INSTANCE_Ord
#define INSTANCE_Enum

#define APPLICATIVE ((->) a)
#include "ApplicativeNumeric-inc.hs"
