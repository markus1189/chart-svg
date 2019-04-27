{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

-- | a spot on the xy-plane
module Chart.Numeric
  ( Spot(..)
  , pattern Point
  , pattern Area
  , Rect(..)
  , pattern Rect
  , Pair(..)
  , pattern Pair
  , toRect
  , toPair
  , projectOn
  , projectTo
  , projectTo2
  , scale
  , rotateRect
  , translateRect
  , Chart.Numeric.widen
  , widenProp
  , rectF
  , dataXY
  , rectXY
  ) where

import NumHask.Data.Pair
import NumHask.Prelude as P hiding (Group)
import NumHask.Data.Rect
import NumHask.Analysis.Space
import Data.Aeson

instance (ToJSON a) => ToJSON (Pair a)
instance (FromJSON a) => FromJSON (Pair a)

-- instance (ToJSON a) => ToJSON (Rect a)
-- instance (FromJSON a) => FromJSON (Rect a)

-- * primitive Chart elements
--- | unification of typical data represented by a chart; a two-dimensional point or a two-dimensional area on the plane
data Spot a =
  Point' (Pair a) |
  Area' (Rect a)
  deriving (Eq, Show, Functor)

-- | pattern for Point x y
pattern Point :: a -> a -> Spot a
pattern Point a b = Point' (Pair a b)
{-# COMPLETE Point #-}

--- | pattern for Area x z y w
pattern Area :: a -> a -> a -> a -> Spot a
pattern Area x z y w = Area' (Rect x z y w)
{-# COMPLETE Area #-}

toRect :: Spot a -> Rect a
toRect (Point x y) = Rect x x y y
toRect (Area x z y w) = Rect x z y w

toPair :: (Divisive a, Additive a, FromInteger a) => Spot a -> Pair a
toPair (Point x y) = Pair x y
toPair (Area x z y w) = Pair ((x+z)/2) ((y+w)/2)

instance (Lattice a) => Semigroup (Spot a) where
  (<>) a b = Area' (toRect a `union` toRect b)

instance (BoundedLattice a) => Monoid (Spot a) where
  mempty = Area' mempty

instance (Additive a) => Additive (Spot a) where
  Point x y + Point x' y' = Point (x+x') (y+y')
  Point x' y' + Area x z y w = Area (x+x') (z+x') (y+y') (w+y')
  Area x z y w + Point x' y' = Area (x+x') (z+x') (y+y') (w+y')
  Area x z y w + Area x' z' y' w' =
    Area (x+x') (z+z') (y+y') (w+w')

  zero = Point zero zero

instance (Lattice a, Subtractive a, Field a) =>
         Multiplicative (Spot a) where
  x * x' = Area' $ Ranges (x0 `rtimes` x1) (y0 `rtimes` y1)
    where
      rtimes a b = bool (Range (m - r/two) (m + r/two)) zero (a == zero || b == zero)
        where
          m = mid a + mid b
          r = width a * width b
      (Ranges x0 y0) = toRect x
      (Ranges x1 y1) = toRect x'

  one = Area' $ Ranges rone rone where
    rone = Range (negate half) half


-- | project a Spot from one Rect to another, preserving relative position.
projectOn :: (Lattice a, Field a, Subtractive a) => Rect a -> Rect a -> Spot a -> Spot a
projectOn new old@(Rect x z y w) po@(Point px py)
  | x==z && y==w = po
  | x==z = Point px py'
  | y==w = Point px' py
  | otherwise = Point px' py'
  where
    (Pair px' py') = project old new (Pair px py)
projectOn new old@(Rect x z y w) ao@(Area ox oz oy ow)
  | x==z && y==w = ao
  | x==z = Area ox oz ny nw
  | y==w = Area nx nz oy ow
  | otherwise = Area' a
  where
    a@(Rect nx nz ny nw) = projectRect old new (toRect ao)

-- | project a [Spot a] from its folded space to the given area
projectTo :: (BoundedLattice a, Field a, Subtractive a) => Rect a -> [Spot a] -> [Spot a]
projectTo vb xs = (projectOn vb (toRect $ fold xs)) <$> xs

-- | project a [[Spot a]] from its folded space to the given area
projectTo2 :: (BoundedLattice a, Field a, Subtractive a) => Rect a -> [[Spot a]] -> [[Spot a]]
projectTo2 vb xss = fmap (projectOn vb (toRect $ fold $ fold <$> xss)) <$> xss

-- | scale a Spot
scale :: (Multiplicative a) => Pair a -> Spot a -> Spot a
scale (Pair x' y') (Point x y) = Point (x*x') (y*y')
scale (Pair x' y') (Area x z y w) = Area (x*x') (z*x') (y*y') (w*y')

-- | widen a Rect by an amount
widen :: (Field a, Subtractive a, FromInteger a) => a -> Rect a -> Rect a
widen a (Rect x z y w) = Rect (x-a/2) (z+a/2) (y-a/2) (w+a/2)

-- | widen an area by a relative proportion
widenProp :: (Multiplicative a, Subtractive a) => a -> Rect a -> Rect a
widenProp p (Rect x z y w) = Rect (x-wid) (z+wid) (y-hei) (w+hei)
  where
    wid = (p - one) * (z - x)
    hei = (p - one) * (w - y)

-- | rotate a pair by x degrees relative to the origin
rotatePair :: (FromInteger a, Subtractive a, TrigField a) => a -> Pair a -> Pair a
rotatePair d (Pair x y) = Pair (x * cos d' + y*sin d') (y* cos d'-x*sin d')
  where
    d' = d*pi/180

-- | the 4 corners of an area
pointsArea :: Rect a -> [Pair a]
pointsArea (Rect x z y w) =
  [ Pair x y
  , Pair x w
  , Pair z y
  , Pair z w
  ]

-- | rotate a Rect by x degrees relative to the origin
rotateRect :: (TrigField a, BoundedLattice a, FromInteger a, Subtractive a) => a -> Rect a -> Rect a
rotateRect d r =
  fold $ (\(Pair x y) -> Rect x x y y) . rotatePair d <$> pointsArea r

-- | translate a Rect
translateRect :: (Additive a) => Pair a -> Rect a -> Rect a
translateRect (Pair x' y') (Rect x z y w) = Rect (x+x') (z+x') (y+y') (w+y')

-- | Create Rect data for a formulae y = f(x) across an x range
rectXY :: (Lattice a, Subtractive a, Field a, FromInteger a) => (a -> a) -> Range a -> Int -> [Rect a]
rectXY f r g = (\x -> Rect (x-tick/two) (x+tick/two) zero (f x)) <$> grid MidPos r g
  where
    tick = NumHask.Analysis.Space.width r / fromIntegral g

-- | Create pair data for a formulae y = f(x) across an x range
dataXY :: (Lattice a, Field a, Subtractive a, FromInteger a) => (a -> a) -> Range a -> Int -> [Pair a]
dataXY f r g = (\x -> Pair x (f x)) <$> grid OuterPos r g

-- | Create area data for a formulae c = f(x,y)
rectF :: (Lattice a, Field a, Subtractive a, FromInteger a) => (Pair a -> b) -> Rect a -> Pair Int -> [(Rect a, b)]
rectF f r g = (\x -> (x, f (mid x))) <$> gridSpace r g
