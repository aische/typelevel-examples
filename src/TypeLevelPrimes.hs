{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  :kind! Example1
  :kind! Primes 100

  or

  :t example1

-}
{-# Language TypeFamilies #-}
{-# Language TypeInType #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
{-# OPTIONS_GHC  -freduction-depth=10000 #-}
-------------------------------------------------------------------------------
module TypeLevelPrimes where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
-------------------------------------------------------------------------------
type family Primes n where
  Primes n = Sieve (Numbers n)

type family Numbers n where
  Numbers 0 = '[]
  Numbers 1 = '[]
  Numbers n = Numbers' (n+1) 2

type family Numbers' n i where
  Numbers' n n = '[]
  Numbers' n i = i ': Numbers' n (i+1)

type family Sieve xs where
  Sieve '[] = '[]
  Sieve (x ': xs) = x ': Sieve (FilterNonDiv x xs)

type family FilterNonDiv x xs where
  FilterNonDiv x '[] = '[]
  FilterNonDiv x (y ': ys) = NonDiv x y y ys

type family NonDiv x y z ys where
  NonDiv x y z ys = NonDiv' (x <=? y) x y z ys

type family NonDiv' isless x y z ys where
  NonDiv' True x x z ys = FilterNonDiv x ys
  NonDiv' False x y z ys = z ': FilterNonDiv x ys
  NonDiv' True x y z ys = NonDiv x (y - x) z ys
-------------------------------------------------------------------------------
type Example1 = Primes 100

example1 :: Proxy Example1
example1 = Proxy
