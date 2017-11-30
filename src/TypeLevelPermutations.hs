{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  :kind! Example1
  :kind! Permutations '[1,2,3,4]

  or

  :t example1

-}
{-# Language TypeFamilies #-}
{-# Language TypeInType #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
-------------------------------------------------------------------------------
module TypeLevelPermutations where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
-------------------------------------------------------------------------------
type family Permutations xs where
  Permutations '[] = '[ '[]]
  Permutations (x ': xs) = InsertAll x (Permutations xs)

type family InsertAll x xs where
  InsertAll x xs = Concat (InsertAllLists x xs)

type family InsertAllLists x ls where
  InsertAllLists x '[] = '[]
  InsertAllLists x (l ': ls) =
    InsertAllPositions x l (Positions 0 l) ': InsertAllLists x ls

type family InsertAllPositions x xs ps where
  InsertAllPositions x xs '[] =
    '[]
  InsertAllPositions x xs (p ': ps) =
    InsertAt p x xs ': InsertAllPositions x xs ps

type family InsertAt n x ys where
  InsertAt 0 x ys = x ': ys
  InsertAt n x (y ': ys) = y ': InsertAt (n-1) x ys

type family Positions n xs where
  Positions n '[] = n ': '[]
  Positions n (x ': xs) = n ': Positions (n+1) xs

type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family Concat xs where
  Concat '[] = '[]
  Concat (x ': xs) = Append x (Concat xs)
-------------------------------------------------------------------------------
type Example1 = Permutations '[1,2,3,4]

example1 :: Proxy Example1
example1 = Proxy
