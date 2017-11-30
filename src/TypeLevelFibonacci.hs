{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  To run the type level fibonacci function, type follwing command in ghci:

  :kind! Example1

  or

  :t example1

-}
-------------------------------------------------------------------------------
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
-------------------------------------------------------------------------------
module TypeLevelFibonacci where

import GHC.TypeLits
import Data.Proxy
-------------------------------------------------------------------------------
type family Fibonacci n where
  Fibonacci n = Fibonacci' n 0 1

type family Fibonacci' n i j where
  Fibonacci' 0 i j = '[]
  Fibonacci' n i j = i ': Fibonacci' (n-1) j (i+j)
-------------------------------------------------------------------------------
type Example1 = Fibonacci 10

example1 :: Proxy Example1
example1 = Proxy
