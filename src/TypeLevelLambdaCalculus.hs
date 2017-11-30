{-
  The Glorious Glasgow Haskell Compilation System, version 8.0.2, OSX 10.10.5

  type level lambda calculus (not a real type level lambda!)

  limitations:
  - no alpha conversion
  - arity of types is limited (currently 3)

  using :kind! will not show the resulting type,
  but example1 has type (Run Example1) and an according value
  (same for example2 and example3)

-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
-------------------------------------------------------------------------------
module TypeLevelLambdaCalculus where

import GHC.TypeLits
-------------------------------------------------------------------------------
data Abs (name :: Symbol) body
data Var (name :: Symbol)
data App fun arg
data Con a

type family Eval term where
  Eval (App fun arg) = Apply (Eval fun) arg
  Eval x = x

type family Apply fun arg where
  Apply (Abs name body) arg = Eval (Replace name arg body)
  Apply fun arg = App fun arg

type family Replace name value term where
  Replace n value (Var n) =
    value
  Replace name value (Var n) =
    Var n
  Replace name value (Abs n body) =
    ReplaceInBody name value (Abs n body)
  Replace name value (App a b) =
    App (Replace name value a) (Replace name value b)
  Replace name value (Con a) =
    Con a

type family ReplaceInBody name value function where
  ReplaceInBody n    value (Abs n body) = (Abs n body)
  ReplaceInBody name value (Abs n body) = Abs n (Replace name value body)

type family Result term where
  Result (App (App (App (Con a) b) c) d) =
    a (Result (Eval b)) (Result (Eval c)) (Result (Eval d))
  Result (App (App (Con a) b) c) =
    a (Result (Eval b)) (Result (Eval c))
  Result (App (Con a) b) =
    a (Result (Eval b))
  Result (Con a) =
    a

type family Run term where
  Run term = Result (Eval term)
-------------------------------------------------------------------------------
type Example1 = App (App (Abs "x" (Abs "y" (Var "x"))) (Con Int)) (Con Bool)

-- const Int Bool
example1 :: Run Example1
example1 = 1

-- applies Either twice to Bool
type Example2 =
  (App
    (App
      (Abs "x"
        (Abs "y"
          (App
            (App
              (Var "y")
              (Var "x"))
            (Var "x"))))
      (Con Bool))
    (Con Either))

example2 :: Run Example2
example2 = Left False

-- adds 2 and 3
type Example3 =
  (App
    (App
      (App
        (Abs "zero"
          (Abs "succ"
            (Abs "add"
              (App
                (App
                  (App
                    (App
                      (Var "add")
                      (App
                        (Var "succ")
                        (App
                          (Var "succ")
                          (Var "zero"))))
                    (App
                      (Var "succ")
                      (App
                        (Var "succ")
                        (App
                          (Var "succ")
                          (Var "zero")))))
                  (Con Maybe)
                )
                (Con Int)))))
        (Abs "f"
          (Abs "x"
            (Var "x"))))
      (Abs "n"
        (Abs "f"
          (Abs "x"
            (App
              (Var "f")
              (App
                (App
                  (Var "n")
                  (Var "f")
                )
                (Var "x")))))))
    (Abs "m"
      (Abs "n"
        (Abs "f"
          (Abs "x"
            (App
              (App
                (Var "m")
                (Var "f"))
              (App
                (App
                  (Var "n")
                  (Var "f"))
                (Var "x"))))))))

example3 :: Run Example3
example3 = Just (Just (Just (Just (Just 3))))
