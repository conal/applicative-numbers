{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Numeric.Vec
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Vector types
----------------------------------------------------------------------

module Data.Numeric.Vec
  ( Vec1(..), Vec2(..), Vec3(..), Vec4(..)
  ) where

import Control.Applicative (Applicative(..),liftA2)

import Text.PrettyPrint.Leijen


{--------------------------------------------------------------------
    Vector types
--------------------------------------------------------------------}

-- | 1D vector
data Vec1 a = Vec1 a

-- | 2D vector
data Vec2 a = Vec2 a a

-- | 3D vector
data Vec3 a = Vec3 a a a

-- | 4D vector
data Vec4 a = Vec4 a a a a

instance Pretty a => Pretty (Vec1 a) where
  pretty (Vec1 a) = pretty a

instance Pretty a => Pretty (Vec2 a) where
  pretty (Vec2 a b) = text "vec2" <$> pretty (a,b)

instance Pretty a => Pretty (Vec3 a) where
  pretty (Vec3 a b c) = text "vec3" <$> pretty (a,b,c)

instance Pretty a => Pretty (Vec4 a) where
  pretty (Vec4 a b c d) = text "vec4" <$> pretty4 (a,b,c,d)

-- instance Pretty a => Show (Vec1 a) where show = show . pretty
-- instance Pretty a => Show (Vec2 a) where show = show . pretty
-- instance Pretty a => Show (Vec3 a) where show = show . pretty
-- instance Pretty a => Show (Vec4 a) where show = show . pretty

-- Dropping the (Pretty a) constraint:

instance Show a => Show (Vec1 a) where
  show (Vec1 a) = show a

instance Show a => Show (Vec2 a) where
  show (Vec2 a b) = "vec2" ++ show (a,b)

instance Show a => Show (Vec3 a) where
  show (Vec3 a b c) = "vec3" ++ show (a,b,c)

instance Show a => Show (Vec4 a) where
  show (Vec4 a b c d) = "vec4" ++ show (a,b,c,d)

{--------------------------------------------------------------------
    Equality instances
--------------------------------------------------------------------}

instance Eq a => Eq (Vec1 a) where
  Vec1 a == Vec1 a'  =  a == a'

instance Eq a => Eq (Vec2 a) where
  Vec2 a b == Vec2 a' b'  =  a == a' && b == b'

instance Eq a => Eq (Vec3 a) where
  Vec3 a b c == Vec3 a' b' c'  =  a == a' && b == b' && c == c'

instance Eq a => Eq (Vec4 a) where
  Vec4 a b c d == Vec4 a' b' c' d'  =  a == a' && b == b' && c == c' && d == d'


{--------------------------------------------------------------------
    Functor & Applicative instances
--------------------------------------------------------------------}

instance Functor Vec1 where
  fmap f (Vec1 a) = Vec1 (f a)
instance Functor Vec2 where
  fmap f (Vec2 a b) = Vec2 (f a) (f b)
instance Functor Vec3 where
  fmap f (Vec3 a b c) = Vec3 (f a) (f b) (f c)
instance Functor Vec4 where
  fmap f (Vec4 a b c d) = Vec4 (f a) (f b) (f c) (f d)


instance Applicative Vec1 where
  pure a = Vec1 a
  Vec1 f <*> Vec1 x = Vec1 (f x)

instance Applicative Vec2 where
  pure a = Vec2 a a
  Vec2 f g <*> Vec2 x y = Vec2 (f x) (g y)

instance Applicative Vec3 where
  pure a = Vec3 a a a
  Vec3 f g h <*> Vec3 x y z = Vec3 (f x) (g y) (h z)

instance Applicative Vec4 where
  pure a = Vec4 a a a a
  Vec4 f g h k <*> Vec4 x y z w = Vec4 (f x) (g y) (h z) (k w)


{--------------------------------------------------------------------
    Numeric instances, via the applicative-numbers package
--------------------------------------------------------------------}

-- Generate Ord & Enum, but not Eq & Show
#define INSTANCE_Ord
#define INSTANCE_Enum

#define APPLICATIVE Vec1
#include "ApplicativeNumeric-inc.hs"

#define APPLICATIVE Vec2
#include "ApplicativeNumeric-inc.hs"

#define APPLICATIVE Vec3
#include "ApplicativeNumeric-inc.hs"

#define APPLICATIVE Vec4
#include "ApplicativeNumeric-inc.hs"



{--------------------------------------------------------------------
    Orphans
--------------------------------------------------------------------}

-- instance (Pretty a,Pretty b,Pretty c,Pretty d) => Pretty (a,b,c,d) where
--   pretty (w,x,y,z)= tupled [pretty w, pretty x, pretty y, pretty z]

pretty4 :: (Pretty a, Pretty b, Pretty c, Pretty d) =>
           (a,b,c,d)-> Doc
pretty4 (w,x,y,z)= tupled [pretty w, pretty x, pretty y, pretty z]
