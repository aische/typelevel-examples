{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  To run the type level register machine, type follwing command in ghci:

  :kind! Example1

  or

  :t example1

-}{-# Language TypeFamilies #-}
{-# Language TypeInType #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
-------------------------------------------------------------------------------
module TypeLevelRM where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
-------------------------------------------------------------------------------
data Inc (n :: Nat)
data Dec (n :: Nat)
data While (n :: Nat) (p :: [Type])
data While' (n :: Nat) (p :: [Type]) (cont :: [Type])

type family IncAt n state where
  IncAt 0 '[]         = 1 ': '[]
  IncAt 0 (n ': rest) = n+1 ': rest
  IncAt i '[]         = 0 ': IncAt (i-1) '[]
  IncAt i (n ': rest) = n ': IncAt (i-1) rest

type family DecAt n state where
  DecAt 0 '[]         = 0 ': '[]
  DecAt 0 (0 ': rest) = 0 ': rest
  DecAt 0 (n ': rest) = (n-1) ': rest
  DecAt i '[]         = 0 ': DecAt (i-1) '[]
  DecAt i (n ': rest) = n ': DecAt (i-1) rest

type family IsAt n state where
  IsAt i '[]         = False
  IsAt 0 (0 ': rest) = False
  IsAt 0 (n ': rest) = True
  IsAt i (n ': rest) = IsAt (i-1) rest

type family EvalWhile stack block state b where
  EvalWhile stack block state True =
    EvalBlock stack block state
  EvalWhile (While' i p cont ': stack) block state False =
    EvalBlock stack cont state

type family EvalBlock stack block state where
  EvalBlock '[] '[] state = state

  EvalBlock (While' i p block ': stack) '[] state =
    EvalBlock stack (While i p ': block) state

  EvalBlock stack (Inc n ': block) state =
    EvalBlock stack block (IncAt n state)

  EvalBlock stack (Dec n ': block) state =
    EvalBlock stack block (DecAt n state)

  EvalBlock stack (While i p ': block) state =
    EvalWhile (While' i p block ': stack) p state (IsAt i state)

type family Run block state where
  Run block state = EvalBlock '[] block state

-------------------------------------------------------------------------------
-- multiply two numbers
type Program1 =
 '[ While 0
    [ Dec 0
    , Inc 2
    , Inc 3
    ]
  , While 2
    [ Dec 2
    , Inc 0
    ]
  , While 3
    [ Dec 3
    , While 1
      [ Dec 1
      , Inc 2
      , Inc 4
      ]
    , While 4
      [ Inc 1
      , Dec 4
      ]
    ]
  ]

type Example1 = Run Program1 '[5,7]

example1 :: Proxy Example1
example1 = Proxy
