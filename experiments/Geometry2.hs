{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Geometry2 where

import Control.Applicative

-- I guess the main difference from vector-space is the lack of
-- AdditiveGroup (Diff p).
class AffineSpace p where
  type Diff p
  (.-.) :: p -> p -> Diff p
  (.+^) :: p -> Diff p -> p

class VectorSpace v where
  type Scalar v
  (*^) :: Scalar v -> v -> v

data Vector a = Vector !a !a deriving Functor

instance Applicative Vector where
  pure x = Vector x x
  Vector x1 y1 <*> Vector x2 y2 = Vector (x1 x2) (y1 y2)

instance Num a => Num (Vector a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

data Point a = Point !a !a deriving Functor

instance AffineSpace a => AffineSpace (Point a) where
  type Diff (Point a) = Vector (Diff a)
  Point x1 y1 .-. Point  x2 y2 = Vector (x1 .-. x2) (y1 .-. y2)
  Point x  y  .+^ Vector dx dy = Point  (x  .+^ dx) (y  .+^ dy)

instance VectorSpace a => VectorSpace (Vector a) where
  type Scalar (Vector a) = Scalar a
  s *^ Vector x y = Vector (s *^ x) (s *^ y)
