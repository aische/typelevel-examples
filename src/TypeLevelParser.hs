{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  simple type level parser

  :kind! Example1
  :kind! Example2
  :kind! Example3
  :kind! Example4
  :kind! Example5
  :kind! Example6
  :kind! Example7

  :t example1
  :t example2
  :t example3
  :t example4
  :t example5
  :t example6
  :t example7

-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
-------------------------------------------------------------------------------
module TypeLevelParser where

import GHC.TypeLits
import Data.Proxy
-------------------------------------------------------------------------------
data Ok s a
data Fail
data Item
data Return a
data Then p f
data Then_ p1 p2
data Or p1 p2
data Elem cs
data Some p
data ReturnF
data ConsF p
data Cons1F x
data Cons x xs
data Nil

type family Parse p s where
  Parse p s = CheckResult (RunP p s)

type family CheckResult r where
  CheckResult (Ok '[] x) = (Ok '[] x)
  CheckResult x = Fail

type family RunP p s where
  RunP Item (x ': xs)   = Ok xs x
  RunP Item '[]         = Fail
  RunP (Return a) xs    = Ok xs a
  RunP (Then p f) xs    = RunThen (RunP p xs) f
  RunP (Then_ p1 p2) xs = RunThen_ (RunP p1 xs) p2
  RunP (Or p1 p2) xs    = RunOr (RunP p1 xs) p2 xs
  RunP (Elem c)   xs    = RunElem c xs
  RunP (Some p)   xs    = RunP (Or (Then p (ConsF (Some p))) (Return Nil)) xs

type family RunElem cs ss where
  RunElem (c ': xs) (c ': ss) = Ok ss c
  RunElem (c ': xs) ss        = RunElem xs ss
  RunElem cs        ss        = Fail

type family RunThen r f where
  RunThen Fail      f          = Fail
  RunThen (Ok xs x) ReturnF    = (Ok xs x)
  RunThen (Ok xs x) (ConsF p)  = RunThen (RunP p xs) (Cons1F x)
  RunThen (Ok xs x) (Cons1F a) = (Ok xs (Cons a x))

type family RunThen_ r f where
  RunThen_ Fail      p = Fail
  RunThen_ (Ok xs x) p = RunP p xs

type family RunOr r p xs where
  RunOr (Ok xs x) p ys = Ok xs x
  RunOr Fail p xs      = RunP p xs

type Many p = Then p (ConsF (Some p))
-------------------------------------------------------------------------------
type IsDigit = Elem ["1", "2", "3"]
type IsAlpha = Elem ["a", "b", "c"]

type Digits = Many IsDigit
type Alphas = Many IsAlpha

-- parses many of alphas, then many digits
type Parser1 = Then Alphas (ConsF (Then Digits (ConsF (Return Nil))))

-- parses many of digits, then many alphas
type Parser2 = Then Digits (ConsF (Then Alphas (ConsF (Return Nil))))

-- parses (parses many of alphas, then many digits)
--     or (many of digits, then many alphas)
type Parser3 = Or Parser1 Parser2

type Inp1 = '["1", "3"]
type Inp2 = '["a", "2", "1"]
type Inp3 = '["2", "3", "a", "b"]
type Inp4 = '["2", "3", "a", "b", "1"]

type Example1 = Parse Parser1 Inp1 -- fail
type Example2 = Parse Parser1 Inp2 -- success
type Example3 = Parse Parser2 Inp3 -- success
type Example4 = Parse Parser2 Inp4 -- fail
type Example5 = Parse Parser3 Inp2 -- success
type Example6 = Parse Parser3 Inp3 -- success
type Example7 = Parse Parser3 Inp4 -- fail

example1 :: Proxy Example1
example1 = Proxy

example2 :: Proxy Example2
example2 = Proxy

example3 :: Proxy Example3
example3 = Proxy

example4 :: Proxy Example4
example4 = Proxy

example5 :: Proxy Example5
example5 = Proxy

example6 :: Proxy Example6
example6 = Proxy

example7 :: Proxy Example7
example7 = Proxy
