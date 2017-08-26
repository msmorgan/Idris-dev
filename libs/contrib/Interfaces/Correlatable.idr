module Interfaces.Correlatable

import Data.Vect

%access public export
%default total

interface Functor f => Correlatable (f : Type -> Type) where
  correlateWith : (a -> b -> c) -> (a -> c) -> (b -> c) -> f a -> f b -> f c

correlate : Correlatable f => f a -> f b -> f (Either (Either a b) (a, b))
correlate xs ys = correlateWith (\x, y => Right (x, y))
                                (\x    => Left (Left x))
                                (\y    => Left (Right y))
                                xs ys

Correlatable List where
  correlateWith f fx fy [] []               = []
  correlateWith f fx fy [] ys               = map fy ys
  correlateWith f fx fy xs []               = map fx xs
  correlateWith f fx fy (x :: xs) (y :: ys) = f x y :: correlateWith f fx fy xs ys

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = map (uncurry f) (rights (correlate xs ys))
  
  
Correlatable Stream where
  correlateWith f fx fy (x :: xs) (y :: ys) = f x y :: correlateWith f fx fy xs ys

Correlatable (Vect n) where
  correlateWith f fx fy [] []               = []
  correlateWith f fx fy (x :: xs) (y :: ys) = f x y :: correlateWith f fx fy xs ys
