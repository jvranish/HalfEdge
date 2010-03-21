{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             MultiParamTypeClasses,
             FunctionalDependencies #-}

module Data.Vector ( Vector1(..)
                   , Vector2(..)
                   , Vector3(..)
                   , Vector4(..)
                   , dot
                   , mag
                   , normalize
                   , angleBetween
                   , scaleBy
                   , scaleTo
                   , direction
                   , distance
                   , projectOnto
                   , cross2
                   , signedAngle2
                   , positiveAngle2
                   , rotate2
                   , cross3
                   , vx
                   , vy
                   , vz
                   , vt
                   , Firstable(..)
                   , Secondable(..)
                   , Thirdable(..)
                   , Fourthable (..)
                   ) where

import Control.Applicative
import Data.Monoid
import Data.Foldable 
import Data.Traversable
import Prelude hiding (sum, fst, snd)

data Vector1 a = Vector1 a deriving (Show, Eq, Ord)
data Vector2 a = Vector2 a a deriving (Show, Eq, Ord)
data Vector3 a = Vector3 a a a deriving (Show, Eq, Ord)
data Vector4 a = Vector4 a a a a deriving (Show, Eq, Ord)

dot :: (Foldable f, Num a, Applicative f, Show (f a), Eq (f a)) =>  f a -> f a -> a
dot a b = sum $ a * b

mag :: (Floating a, Foldable f, Applicative f, Show (f a), Eq (f a)) => f a -> a
mag a = sqrt $ a `dot` a

normalize :: (Floating a, Foldable f, Applicative f, Show (f a), Eq (f a)) => f a -> f a
normalize a = let m = mag a in if m /= 0 then a / pure m else a

angleBetween :: (Floating a, Foldable f, Applicative f, Show (f a), Eq (f a)) =>  f a -> f a -> a
angleBetween a b = acos $ dot a b

scaleBy :: (Num a, Applicative f) => f a -> a -> f a
scaleBy a b = pure (b *) <*> a

scaleTo :: (Floating a, Foldable f, Applicative f, Show (f a), Eq (f a)) => f a -> a -> f a
scaleTo a b = a `scaleBy` (b / (mag a))

direction :: (Floating a, Foldable f, Applicative f, Show (f a), Eq (f a)) => f a -> f a -> f a
direction a b = normalize $ b - a

distance :: (Floating a, Foldable f, Applicative f, Show (f a), Eq (f a)) => f a -> f a -> a
distance a b = mag $ b - a

projectOnto :: (Foldable f, Fractional a, Applicative f, Show (f a), Eq (f a)) => f a -> f a -> f a
projectOnto a b = b `scaleBy` ((a `dot` b)/(b `dot` b))

cross2 :: (Num a) => Vector2 a -> Vector2 a -> a
cross2 (Vector2 ax ay) (Vector2 bx by) = (ax * by - ay * bx)

signedAngle2 :: (RealFloat a) => Vector2 a -> Vector2 a -> a
signedAngle2 a' b' = atan2 (a `cross2` b) (a `dot` b)
  where
    a = normalize a'
    b = normalize b'
    
positiveAngle2 :: (RealFloat a) => Vector2 a -> Vector2 a -> a
positiveAngle2 a b = if angle < 0 then 2 * pi + angle else angle
  where
    angle = signedAngle2 a b

rotate2 :: (Floating a) => Vector2 a -> a -> Vector2 a
rotate2 (Vector2 x y) angle = Vector2 (x * c - y * s) (y * c + x * s)
  where
    c = cos angle
    s = sin angle
    
-- cross product is generalizable to N dimensions, but I'm not going to do it :(
-- well, maybe but it will be called perpendicular and will be in the Matrix module
cross3 :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
cross3 (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

-- If this causes problems for someone, let me know
instance (Applicative v, Num e, Show (v e), Eq (v e)) => Num (v e) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Applicative v, Fractional e, Show (v e), Eq (v e)) => Fractional (v e) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

instance Functor Vector1 where
  fmap f (Vector1 x) = Vector1 (f x)
instance Functor Vector2 where
  fmap f (Vector2 x y) = Vector2 (f x) (f y)
instance Functor Vector3 where
  fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
instance Functor Vector4 where
  fmap f (Vector4 x y z w) = Vector4 (f x) (f y) (f z) (f w)

instance Applicative Vector1 where
  pure x = Vector1 x
  Vector1 f <*> Vector1 x = Vector1 (f x)
instance Applicative Vector2 where
  pure x = Vector2 x x
  Vector2 f g <*> Vector2 x y = Vector2 (f x) (g y)
instance Applicative Vector3 where
  pure x = Vector3 x x x
  Vector3 f g h <*> Vector3 x y z = Vector3 (f x) (g y) (h z)
instance Applicative Vector4 where
  pure x = Vector4 x x x x
  Vector4 f g h i <*> Vector4 x y z w = Vector4 (f x) (g y) (h z) (i w)

instance Foldable Vector1 where
  foldMap f (Vector1 a) = f a  
instance Foldable Vector2 where
  foldMap f (Vector2 a b) = f a `mappend` f b
instance Foldable Vector3 where
  foldMap f (Vector3 a b c) = f a `mappend` f b `mappend` f c
instance Foldable Vector4 where
  foldMap f (Vector4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d
  
instance Traversable Vector1 where
  traverse f (Vector1 a) = Vector1 <$> f a
instance Traversable Vector2 where
  traverse f (Vector2 a b) = Vector2 <$> f a <*> f b
instance Traversable Vector3 where
  traverse f (Vector3 a b c) = Vector3 <$> f a <*> f b <*> f c
instance Traversable Vector4 where
  traverse f (Vector4 a b c d) = Vector4 <$> f a <*> f b <*> f c <*> f d

vx :: (Firstable a b) => a -> b
vx = fst
vy :: (Secondable a b) => a -> b
vy = snd
vz :: (Thirdable a b) => a -> b
vz = thrd
vt :: (Fourthable a b) => a -> b
vt = frth

class Firstable a b | a -> b where
  fst :: a -> b
  
class Secondable a b | a -> b where
  snd :: a -> b

class Thirdable a b | a -> b where
  thrd :: a -> b

class Fourthable a b | a -> b where
  frth :: a -> b
  
instance Firstable (a, b) a where
  fst (a, _) = a
  
instance Firstable (a, b, c) a where
  fst (a, _, _) = a

instance Firstable (a, b, c, d) a where
  fst (a, _, _, _) = a
  
instance Secondable (a, b) b where
  snd (_, a) = a

instance Secondable (a, b, c) b where
  snd (_, a, _) = a
  
instance Secondable (a, b, c, d) b where
  snd (_, a, _, _) = a

instance Thirdable (a, b, c) c where
  thrd (_, _, a) = a

instance Thirdable (a, b, c, d) c where
  thrd (_, _, a, _) = a

instance Fourthable (a, b, c, d) d where
  frth (_, _, _, a) = a    

instance Firstable (Vector1 a) a where
  fst (Vector1 a) = a
  
instance Firstable (Vector2 a) a where
  fst (Vector2 a _) = a

instance Firstable (Vector3 a) a where
  fst (Vector3 a _ _) = a

instance Firstable (Vector4 a) a where
  fst (Vector4 a _ _ _) = a
    
instance Secondable (Vector2 a) a where
  snd (Vector2 _ a) = a

instance Secondable (Vector3 a) a where
  snd (Vector3 _ a _) = a
  
instance Secondable (Vector4 a) a where
  snd (Vector4 _ a _ _) = a

instance Thirdable (Vector3 a) a where
  thrd (Vector3 _ _ a) = a

instance Thirdable (Vector4 a) a where
  thrd (Vector4 _ _ a _) = a

instance Fourthable (Vector4 a) a where
  frth (Vector4 _ _ _ a) = a


