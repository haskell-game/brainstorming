{-# LANGUAGE FlexibleInstances #-}
module Geometry where

import Control.Applicative

-- jmcarthur: After thinking about it a bit, here is my opinion on this.
-- If you imagine a 2D plot where one axis is type safety and the other is
-- straightforwardness, doing nothing interesting would be close to the (0,1)
-- extreme, going all out would be close to the (1,0) extreme, and I think
-- this approach would sit around (4,4). That's okay, but I think it is more
-- important that the interface be simple than that it be type safe. We are
-- shooting for this being as approachable as possible. Most bugs this prevents
-- are going to be immediately visible at runtime anyway; there is little risk
-- of them lurking and only biting after you have long forgotten about the code.
-- Also, being merely phantom types, this could just be implemented on top
-- anyway, if somebody wants it, and then they get to do it however they want.
{-

The goal here is to make as many potential bugs into type errors as possible
without too many trade-offs. The unfortunate fact is that we don't have enough
expressive power to make everything work all the time, so rather than true 
type-enforced correctness the emphasis is on making it easy to use types to
keep track of what things should be.

As such, it avoids:
- Needlessly duplicated implementations
- Type hackery and the pile of GHC extensions that comes with
- Lots of conversions needed for common tasks
- Restrictions the user can't circumvent if they need to

The basic approach here is to have a single representation with a phantom type
tag and an exported constructor. Functions can be specialized with the phantom
type, including a sort of hierarchy by leaving some polymorphic bits in.

The details of the phantom type information are flexible and could easily be
revised from what's used here

-}

-- jmcarthur for cmccann: Why not have a separate phantom type for each
-- property instead of using phantom type constructors? Also, what does Cd2
-- stand for?
--     data Cd2 c s u a = Cd2 !a !a
data Cd2 t a = Cd2 !a !a
  deriving (Eq, Ord, Read, Show)

-- phantom types identifying what the coordinates actually mean
data Vec a 
data Point a

-- phantom types identifying what space the coordinates are in
data Screen a
data GameWorld a

-- phantom types identifying the unit of measurement
data Pixel
data Tile
data GLUnits


instance Functor (Cd2 t) where
    fmap f (Cd2 x y) = Cd2 (f x) (f y)

instance Applicative (Cd2 t) where
    pure x = Cd2 x x
    Cd2 f1 f2 <*> Cd2 x1 x2 = Cd2 (f1 x1) (f2 x2)

-- Num instance is zippy applicative style. other type classes can be similar.
-- note the restriction on the phantom type; arithmetic on points doesn't 
-- make a lot of sense.
instance (Num a) => Num (Cd2 (Vec t) a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

data Rect t a = Rect !(Cd2 (Point t) a) !(Cd2 (Vec t) a)

{- 
example: 'Rect (Screen Pixel)' is a rectangle measured in pixels on the screen, 
e.g. the viewport window. it's composed of two values with types 
'Cd2 (Point (Screen Pixel))' and 'Cd2 (Vec (Screen Pixel))' representing the 
top-left corner and the size or something.
-}

-- this allows semi-generic functions that restrict some part of the phantom 
-- type parameter but leave others polymorphic
pixelsToTiles :: (Integral a) => Cd2 (t (s Pixel)) a -> Cd2 (t (s Tile)) a
pixelsToTiles (Cd2 x y) = Cd2 (x `mod` tileSize) (y `mod` tileSize)
  where tileSize = 32 -- this would come from somewhere else in actual use ofc



