{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  To run the type level turing machine, type follwing command in ghci:

  :kind! Example1
  :kind! Example2

  or

  :t example1
  :t example2

-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
-------------------------------------------------------------------------------
module TypeLevelTM where

import GHC.TypeLits
import Data.Proxy
-------------------------------------------------------------------------------
data Rule state input output direction newstate

data Operation output direction state

type family FindRule state input rules where
  FindRule s     i (Rule s i o d n ': rules) = Operation o d n
  FindRule state input (Rule s i o d n ': rules) = FindRule state input rules

type family Step rules state l current r where
  Step rules 0     l current r = Result l (current ': r)
  Step rules state l current r = Step' rules (FindRule state current rules) l r

type family Step' rules operation l r where
  Step' rules (Operation out True nextstate) l '[] = Step rules nextstate (out ': l) 0 '[]
  Step' rules (Operation out True nextstate) l (r ': rs) = Step rules nextstate (out ': l) r rs

  Step' rules (Operation out False nextstate) '[] r = Step rules nextstate '[] 0 (out ': r)
  Step' rules (Operation out False nextstate) (l ': ls) r = Step rules nextstate ls l (out ': r)

type family Result l r where
  Result '[] r = r
  Result (l ': ls) r = Result ls (l ': r)

type family Run state band rules where
  Run state (x ': xs) rules = Step rules state '[] x xs
-------------------------------------------------------------------------------
-- add two numbers
type Program1 =
  [ Rule 1   0   0   True    8
  , Rule 1   1   0   True    2
  , Rule 2   0   0   True    3
  , Rule 2   1   1   True    2
  , Rule 3   0   0   True    4
  , Rule 3   1   1   True    3
  , Rule 4   0   1   False   5
  , Rule 4   1   1   True    4
  , Rule 5   0   0   False   6
  , Rule 5   1   1   False   5
  , Rule 6   0   0   False   7
  , Rule 6   1   1   False   6
  , Rule 7   0   1   True    1
  , Rule 7   1   1   False   7
  , Rule 8   0   0   True    0
  , Rule 8   1   0   True    9
  , Rule 9   0   0   True    10
  , Rule 9   1   1   True    9
  , Rule 10  0   1   False   11
  , Rule 10  1   1   True    10
  , Rule 11  0   0   False   12
  , Rule 11  1   1   False   11
  , Rule 12  0   1   True    8
  , Rule 12  1   1   False   12
  ]

type Tape1 = '[1,1,1,1,1,0,1,1,1]

type Example1 = Run 1 Tape1 Program1

example1 :: Proxy Example1
example1 = Proxy

-- multiply two numbers
type Program2 =
  [ Rule 1   0   0   False   10
  , Rule 1   1   0   True    2
  , Rule 2   0   0   True    3
  , Rule 2   1   1   True    2
  , Rule 3   0   0   False   8
  , Rule 3   1   0   True    4
  , Rule 4   0   0   True    5
  , Rule 4   1   1   True    4
  , Rule 5   0   1   False   6
  , Rule 5   1   1   True    5
  , Rule 6   0   0   False   7
  , Rule 6   1   1   False   6
  , Rule 7   0   1   True    3
  , Rule 7   1   1   False   7
  , Rule 8   0   0   False   9
  , Rule 8   1   1   False   8
  , Rule 9   0   1   True    1
  , Rule 9   1   1   False   9
  , Rule 10  0   0   True    0
  , Rule 10  1   1   False   10
  ]

type Tape2 = '[1,1,1,1,1,0,1,1,1]

type Example2 = Run 1 Tape2 Program2

example2 :: Proxy Example2
example2 = Proxy
