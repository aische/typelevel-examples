{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  To run the type level quicksort, type follwing command in ghci:

  :kind! Example1

  :set -XDataKinds
  :kind! Quicksort '["h", "a", "s", "k", "e", "l", "l"]

  or

  :t example1

-}
-------------------------------------------------------------------------------
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
-------------------------------------------------------------------------------
module TypeLevelQuicksort where

import GHC.TypeLits
import Data.Proxy
-------------------------------------------------------------------------------
type family Quicksort t where
  Quicksort '[] = '[]
  Quicksort (x ': xs) =
    Append
      (Quicksort (FilterLower x xs))
      (x ': (Quicksort (FilterHigher x xs)))

type family FilterLower s l where
  FilterLower s '[] = '[]
  FilterLower s (x ': xs) = Lower s x (CmpSymbol x s) xs

type family Lower s x lt xs where
  Lower s x LT xs = x ': FilterLower s xs
  Lower s x GT xs = FilterLower s xs
  Lower s x EQ xs = FilterLower s xs

type family FilterHigher s l where
  FilterHigher s '[] = '[]
  FilterHigher s (x ': xs) = Higher s x (CmpSymbol x s) xs

type family Higher s x lt xs where
  Higher s x LT xs = FilterHigher s xs
  Higher s x GT xs = x ': FilterHigher s xs
  Higher s x EQ xs = x ': FilterHigher s xs

type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys
-------------------------------------------------------------------------------
type Example1 =
  Quicksort '["type", "level", "programming", "examples", "in", "haskell"]

example1 :: Proxy Example1
example1 = Proxy
