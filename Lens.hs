{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Lens where

import Prelude hiding (sum)
import Data.Monoid
import Control.Applicative
import qualified Data.Traversable as T

-- based on https://www.codewars.com/kata/54258ffb430ca2e4b5000239

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> (p a b -> p a' b')
  dimap f g = lmap f . rmap g
  lmap ::  (a' -> a) -> (p a b -> p a' b)
  lmap f = dimap f id
  rmap ::  (b -> b') -> (p a b -> p a b')
  rmap f = dimap id f
  
class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)
  
instance Profunctor (->) where
  dimap f g h = g . h . f
  
instance Choice (->) where
  left'  f = either (Left . f) Right
  right' f = either Left (Right . f)
  
class Contravariant f where
  contramap :: (a' -> a) -> (f a -> f a')

newtype K b a = K { getK :: b } deriving Functor

instance Monoid b => Applicative (K b) where
  pure _ = K mempty
  K e <*> K f = K (e <> f)

instance Contravariant (K b) where
  contramap f (K b) = K b

newtype Id a = Id { getId :: a } deriving Functor

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)
  
-- | Optic is the general pattern for all other lens types.
type Optic p f s t a b =
  p a (f b) -> p s (f t)
  
type Iso s t a b =
  forall p f . (Profunctor p, Functor f) =>
  Optic p f s t a b
  
type Lens s t a b =
  forall f . Functor f => 
  Optic (->) f s t a b
  
type Traversal s t a b =
  forall f . Applicative f =>
  Optic (->) f s t a b
  
type Fold s a = 
  forall f . (Contravariant f, Applicative f) =>
  Optic (->) f s s a a

type Prism s t a b =
  forall p f . (Choice p, Applicative f) =>
  Optic p f s t a b
    
-- | A lens focusing on the first element in a pair
_1 :: Lens (a, x) (b, x) a b
_1 = \f -> \(a,x) -> fmap (,x) (f a) 

-- | A lens focusing on the second element in a pair
_2 :: Lens (x, a) (x, b) a b
_2 = \f -> \(x,a) -> fmap (x,) (f a) 

-- | A function which takes a lens and looks through it.
view :: Optic (->) (K a) s t a b -> (s -> a)
view opt = getK . opt K       

-- | A function which takes a lens and a transformation function
-- and applies that transformer at the focal point of the lens.
over :: Optic (->) Id s t a b -> (a -> b) -> (s -> t)
over opt f = getId . opt (fmap f . Id)   

-- | A function from a lens and a value which sets the value
-- at the focal point of the lens. 
set :: Optic (->) Id s t a b -> b -> (s -> t)
set opt b = getId . opt (\_ -> Id b) 

-- | A traversal which focuses on each element in any 
-- Traversable container.
elements :: T.Traversable f => Traversal (f a) (f b) a b
elements f = \s -> T.sequenceA $ fmap f s    

-- | A function which takes a Traversal and pulls out each 
-- element it focuses on in order. 
toListOf :: Optic (->) (K (Endo [a])) s s a a -> (s -> [a])
toListOf opt =
  \s -> (appEndo $ getK $ opt (\a -> K $ Endo $ (a:)) s) [] 

-- | A function which takes any kind of Optic which might
-- be focused on zero subparts and returns Just the first
-- subpart or else Nothing.
preview :: Optic (->) (K (First a)) s s a a -> (s -> Maybe a)
preview opt = \s -> getFirst $ getK $ opt (K . First . Just) s

coerce :: (Contravariant f, Functor f) => f a -> f b
coerce x = g () (() <$ x)
  where g = contramap . const

-- | A Fold which views the result of a function application
to :: (a -> b) -> Fold a b
to f = \g -> \s -> coerce $ g (f s)

-- | A prism which focuses on the left branch of an Either
_Left :: Prism (Either a x) (Either b x) a b
_Left = \p -> rmap g (left' p)
  where g (Right x) = pure (Right x)
        g (Left x)  = fmap Left x
  
-- | A prism which focuses on the right branch of an Either
_Right :: Prism (Either x a) (Either x b) a b
_Right = \p -> rmap g (right' p)
  where g (Right x) = fmap Right x
        g (Left x)  = pure (Left x)

-- | An iso which witnesses that tuples can be flipped without
-- losing any information
_flip :: Iso (a, b) (a, b) (b, a) (b, a)
_flip = \p -> dimap g (fmap g) p
  where g (a,b) = (b,a)
