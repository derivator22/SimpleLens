{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExampleLenses where

import Lens

data Person = Person { name :: String, age :: Int } deriving (Eq, Show)

type Lens' s a = Lens s s a a

-- a lens focused on the name inside a person record
_name :: Lens' Person String
_name f = \(Person n a) -> fmap (\x -> Person x a) (f n)


newtype TempC = TempC { getC :: Float } deriving (Eq, Show, Num)
newtype TempF = TempF { getF :: Float } deriving (Eq, Show, Num)
c_f (TempC c) = TempF $ (9/5 * c) + 32
f_c (TempF f) = TempC $ 5/9 * (f - 32)

-- this lens focuses on the Celsius temp "inside" a Fahrenheit temp.
-- view _celsius (TempF 12)
_celsius :: Lens' TempF TempC
_celsius f = \tf -> fmap c_f (f $ f_c tf)   

-- Lens Composition

-- make a lens focused on the name of a person in a nested tuple
_1_1_1_name :: Lens' (((Person, x), y), z) String
_1_1_1_name = _1 . _1 . _1 . _name

-- Automatic Lens Generator

-- `lens` can generate a `Lens s t a b` from a getter and setter
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = \f -> \s -> fmap (setter s) (f $ getter s)


