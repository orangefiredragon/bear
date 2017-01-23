{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Serv.Util.GenericMonoid where

import           Data.Monoid    hiding ((<>))
import           Data.Semigroup
import           GHC.Generics

gmempty :: (Generic a, GMonoid (Rep a)) => a
gmempty = to gmempty'

class GSemigroup f => GMonoid f where
    gmempty' :: f p

instance (Semigroup a, Monoid a) => GMonoid (K1 i a) where
    gmempty' = K1 mempty

instance GMonoid f => GMonoid (M1 i c f) where
    gmempty' = M1 gmempty'

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
    gmempty' = gmempty' :*: gmempty'


-- Stolen from Edward Kmett's BSD3-licensed `semigroups` package

gmappend :: (Generic a, GSemigroup (Rep a)) => a -> a -> a
gmappend x y = to (gmappend' (from x) (from y))

class GSemigroup f where
    gmappend' :: f p -> f p -> f p

instance Semigroup a => GSemigroup (K1 i a) where
    gmappend' (K1 x) (K1 y) = K1 (x <> y)

instance GSemigroup f => GSemigroup (M1 i c f) where
    gmappend' (M1 x) (M1 y) = M1 (gmappend' x y)

instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
    gmappend' (x1 :*: x2) (y1 :*: y2) = gmappend' x1 y1 :*: gmappend' x2 y2
