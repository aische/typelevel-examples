{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  type level combinatory logic


  using :kind! will not show the resulting type,
  but example1 has type (Run Example1) and an according value

-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
-------------------------------------------------------------------------------
module TypeLevelSKI where

import GHC.TypeLits
import Data.Proxy
-------------------------------------------------------------------------------
data V a
data I
data K
data S
data A f x

type family Eval term where
  Eval (V a) = V a
  Eval I = I
  Eval K = K
  Eval S = S
  Eval (A I x) = Eval x
  Eval (A K x) = A K (Eval x)
  Eval (A S x) = A S (Eval x)
  Eval (A (A K x) y) = Eval x
  Eval (A (A I x) y) = Eval (A (Eval x) (Eval y))
  Eval (A (A S x) y) = (A (A S (Eval x)) (Eval y))
  Eval (A (A (A S f) g) x) = Eval (A (A (Eval f) (Eval x)) (Eval (A g x)))
  Eval (A (A (A I f) g) x) = Eval (A (A f g) x)
  Eval (A (A (A K f) g) x) = Eval (A f x)
  Eval (A (A (A (A a b) c) d) e) = Eval (A (Eval (A (A (A a b) c) d)) e)
  Eval x = x

type family Result t where
  Result (A (A (V t) x) y) = t (Result x) (Result y)
  Result (A (V t) x) = t (Result x)
  Result (V t) = t

type family Run t where
  Run t = Result (Eval t)
-------------------------------------------------------------------------------
-- adds 2 and 3
type Example1 =
  (A (A (A (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) (A (A S (A K (A S (A K (A S I))))) (A (A S (A K (A S (A K K)))) (A (A S (A K (A S I))) (A (A S (A K (A S I))) (A (A S (A K K)) I)))))))) (A (A S (A K (A S (A K K)))) (A (A S (A K (A S I))) (A (A S (A K (A S I))) (A (A S (A K (A S I))) (A (A S (A K K)) I))))))))) (A K (A K (A K (V Maybe)))))))) (A K (A K (A K (V Int))))) (A K I)) (A (A S (A K (A S (A (A S (A K S)) (A (A S (A K K)) I))))) (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) (A (A S (A K (A S (A K K)))) (A (A S (A (A S (A K S)) (A (A S (A K K)) I))) (A K I)))))) (A K (A K I))))) (A (A S (A (A S (A K S)) (A (A S (A K K)) (A (A S (A K S)) (A (A S (A K (A S (A K S)))) (A (A S (A K (A S (A K K)))) (A (A S (A (A S (A K S)) (A (A S (A K K)) I))) (A K I)))))))) (A K (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) (A (A S (A K (A S (A K K)))) (A (A S (A (A S (A K S)) (A (A S (A K K)) I))) (A K I)))))) (A K (A K I))))))

example1 :: Run Example1
example1 = Just (Just (Just (Just (Just 3))))

type E = Run Example1