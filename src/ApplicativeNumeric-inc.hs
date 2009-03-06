{-----------------------------------------------------------------------
Meta-Module :  ApplicativeNumeric-inc
Copyright   :  (c) Conal Elliott 2008
License     :  GPL-3

Maintainer  :  conal@conal.net
Stability   :  experimental

Instances of Num classes for applicative functors.  To be #include'd after
defining APPLICATIVE as the applicative functor name and CONSTRAINTS as a
list of constraints, which must carry its own trailing comma if non-empty.
The APPLICATIVE symbol gets #undef'd at the end of this include file, so
that multiple includes are convenient.

For instance,

    -- Generate Ord & Enum, but not Eq & Show
    #define INSTANCE_Ord
    #define INSTANCE_Enum

    #define APPLICATIVE Vec2
    #include "ApplicativeNumeric-inc.hs"

    #define APPLICATIVE Vec3
    #include "ApplicativeNumeric-inc.hs"

    #define APPLICATIVE Vec4
    #include "ApplicativeNumeric-inc.hs"


You'll also have to import 'pure' and 'liftA2' from "Control.Applicative"
and specify the FlexibleContexts language extension (due to a hack below).

Some instances are generated only if a corresponding CPP symbol is
defined:

+ INSTANCE_Eq
+ INSTANCE_Ord
+ INSTANCE_Show
+ INSTANCE_Enum

-----------------------------------------------------------------------}

#ifndef CONSTRAINTS
#define CONSTRAINTS
#endif

#ifndef noOv_DEFINED
noOv :: String -> a
noOv meth = error $ meth ++ ": No overloading"
#define noOv_DEFINED
#endif

-- TODO: splice APPLICATIVE into the error message.  I don't have the CPP chops.

-- Eq & Show are prerequisites for Num, so they have to be provided somehow

-- Hack: the Functor [] is a no-op that allows for a ","-terminated CONSTRAINTS
-- Requires FlexibleContexts
#ifdef INSTANCE_Eq
instance (CONSTRAINTS Functor []) => Eq (APPLICATIVE f9ksE) where (==) = noOv "(==)"
#endif

#ifdef INSTANCE_Ord
instance (CONSTRAINTS Ord f9ksE) => Ord (APPLICATIVE f9ksE) where
  { min = liftA2 min ; max = liftA2 max }
#endif

#ifdef INSTANCE_Show
instance Show (APPLICATIVE f9ksE) where
  { show      = noOv "show"
  ; showsPrec = noOv "showsPrec"
  ; showList  = noOv "showList"
  }
#endif

#ifdef INSTANCE_Enum
instance (CONSTRAINTS Enum f9ksE) => Enum (APPLICATIVE f9ksE) where
  { succ           = fmap succ
  ; pred           = fmap pred
  ; toEnum         = pure . toEnum
  ; fromEnum       = noOv "fromEnum"
  ; enumFrom       = noOv "enumFrom"
  ; enumFromThen   = noOv "enumFromThen"
  ; enumFromTo     = noOv "enumFromTo"
  ; enumFromThenTo = noOv "enumFromThenTo"
  }
#endif

instance (CONSTRAINTS Num f9ksE) => Num (APPLICATIVE f9ksE) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance (CONSTRAINTS Num f9ksE, Ord f9ksE) => Real (APPLICATIVE f9ksE) where
  toRational = noOv "toRational"

instance (CONSTRAINTS Integral f9ksE) => Integral (APPLICATIVE f9ksE) where
  quot          = liftA2 quot
  rem           = liftA2 rem
  div           = liftA2 div
  mod           = liftA2 mod
  toInteger     = noOv "toInteger"
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod`  y = (x `div`  y, x `mod` y)

instance (CONSTRAINTS Fractional f9ksE) => Fractional (APPLICATIVE f9ksE) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (CONSTRAINTS Floating f9ksE) => Floating (APPLICATIVE f9ksE) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance (CONSTRAINTS RealFrac f9ksE) => RealFrac (APPLICATIVE f9ksE) where
  properFraction = noOv "properFraction"
  truncate       = noOv "truncate"
  round          = noOv "round"
  ceiling        = noOv "ceiling"
  floor          = noOv "floor"

instance (CONSTRAINTS RealFloat f9ksE) => RealFloat (APPLICATIVE f9ksE) where
  floatRadix     = noOv "floatRadix"
  floatDigits    = noOv "floatDigits"
  floatRange     = noOv "floatRange"
  decodeFloat    = noOv "decodeFloat"
  encodeFloat    = ((.).(.)) pure encodeFloat
  exponent       = noOv "exponent"
  significand    = noOv "significand"
  scaleFloat n   = fmap (scaleFloat n)
  isNaN          = noOv "isNaN"
  isInfinite     = noOv "isInfinite"
  isDenormalized = noOv "isDenormalized"
  isNegativeZero = noOv "isNegativeZero"
  isIEEE         = noOv "isIEEE"
  atan2          = liftA2 atan2

#undef APPLICATIVE
