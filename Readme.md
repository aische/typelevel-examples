# Haskell Type Level Example Programs

Open ghci and load the one of the examples:


	stack ghci
	
	ghci> :l src/TypeLevelFibonacci.hs


All programs are written as Types. Given a program called "Example1", it can be run by typing

    ghci> :kind! Example1
    
In most cases, for a program called "Example1" there is also a value called "example1" with type "Proxy Example1". It can be run by typing

	ghci> :t example1

For some examples you might have to increase the reduction depth:

	ghci> :set -freduction-depth=10000
or

	ghci> :set -freduction-depth=0
		
Some examples will not show the fully evaluated result type by using ":kind!". In these cases a value is defined that uses the result type in its type signature to check if the result type is what we expect.
	
## Fibonacci

Compute fibonacci numbers

	type family Fibonacci n where
	  Fibonacci n = Fibonacci' n 0 1

	type family Fibonacci' n i j where
	  Fibonacci' 0 i j = '[]
	  Fibonacci' n i j = i ': Fibonacci' (n-1) j (i+j)

	type Example1 = Fibonacci 10

	example1 :: Proxy Example1
	example1 = Proxy

Typing ":kind! Example1" will result in

	Example1 :: [Nat] = '[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

Typing ":t example1" will result in

	example1 :: Proxy '[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

	
## Quicksort

Sorting lists of symbols.

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

    type Example1 =
      Quicksort '["type", "level", "programming", "examples", "in", "haskell"]

    example1 :: Proxy Example1
    example1 = Proxy

Typing ":kind! Example1" will result in

	Example1 :: [Symbol] = '["examples", "haskell", "in", "level", "programming", "type"]

## Permutations

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

    type Example1 = Permutations '[1,2,3,4]

    example1 :: Proxy Example1
    example1 = Proxy

Typing ":kind! Example1" will result in

    Example1 :: [[Nat]] 
    = '['[1, 2, 3, 4], '[2, 1, 3, 4], '[2, 3, 1, 4], '[2, 3, 4, 1],
        '[1, 3, 2, 4], '[3, 1, 2, 4], '[3, 2, 1, 4], '[3, 2, 4, 1],
        '[1, 3, 4, 2], '[3, 1, 4, 2], '[3, 4, 1, 2], '[3, 4, 2, 1],
        '[1, 2, 4, 3], '[2, 1, 4, 3], '[2, 4, 1, 3], '[2, 4, 3, 1],
        '[1, 4, 2, 3], '[4, 1, 2, 3], '[4, 2, 1, 3], '[4, 2, 3, 1],
        '[1, 4, 3, 2], '[4, 1, 3, 2], '[4, 3, 1, 2], '[4, 3, 2, 1]]
        
## Primes

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

    type Example1 = Primes 100

    example1 :: Proxy Example1
    example1 = Proxy

Typing ":kind! Example1" will result in
	
	Example1 :: [Nat]
	= '[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59,
	    61, 67, 71, 73, 79, 83, 89, 97]
	
	
## Turing Machine

The program run by this turing machine adds two numbers.

    data Rule state input output direction newstate

    data Operation output direction state

    type family FindRule state input rules where
      FindRule s     i (Rule s i o d n ': rules) = Operation o d n
      FindRule state input (Rule s i o d n ': rules) = FindRule state input rules

    type family Step rules state l current r where
      Step rules 0     l current r = Result l (current ': r)
      Step rules state l current r = Step' rules (FindRule state current rules) l r

    type family Step' rules operation l r where
      Step' rules (Operation out True nextstate) l '[] = 
      	Step rules nextstate (out ': l) 0 '[]
      Step' rules (Operation out True nextstate) l (r ': rs) = 
      	Step rules nextstate (out ': l) r rs
      Step' rules (Operation out False nextstate) '[] r = 
      	Step rules nextstate '[] 0 (out ': r)
      Step' rules (Operation out False nextstate) (l ': ls) r = 
      	Step rules nextstate ls l (out ': r)

    type family Result l r where
      Result '[] r = r
      Result (l ': ls) r = Result ls (l ': r)

    type family Run state band rules where
      Run state (x ': xs) rules = Step rules state '[] x xs

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

Typing ":kind! Example1" will result in

    Example1 :: [Nat]
    = '[1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1]
    
## Register Machine

The program run by this register machine muliplies two numbers.

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

Typing ":kind! Example1" will result in

	Example1 :: [Nat] = '[5, 7, 35, 0, 0]
	
## SKI Combinators

The Example1 program adds two numbers, 2 and 3. Unfortunately the command ":kind! Example1" won't show the fully evaluated result. To check if the resulting type is what we expect, we define the value "example1". We give "Maybe" as successor function and "Int" as zero, and give "example1" the type "Run Example1" and a value of (Just (Just (Just (Just (Just 3))))). If the resulting type did not match, the type checker would complain.

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

    type Example1 =
      (A (A (A (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) 
        (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) 
          (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) 
            (A (A S (A K (A S (A K (A S I))))) (A (A S (A K (A S (A K K)))) 
              (A (A S (A K (A S I))) (A (A S (A K (A S I))) 
                (A (A S (A K K)) I)))))))) 
          (A (A S (A K (A S (A K K)))) (A (A S (A K (A S I))) (A (A S (A K (A S I))) 
            (A (A S (A K (A S I))) (A (A S (A K K)) I))))))))) 
        (A K (A K (A K (V Maybe)))))))) (A K (A K (A K (V Int))))) (A K I)) 
      (A (A S (A K (A S (A (A S (A K S)) (A (A S (A K K)) I))))) 
        (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) 
          (A (A S (A K (A S (A K K)))) (A (A S (A (A S (A K S)) 
            (A (A S (A K K)) I))) (A K I)))))) (A K (A K I))))) 
      (A (A S (A (A S (A K S)) (A (A S (A K K)) (A (A S (A K S)) 
        (A (A S (A K (A S (A K S)))) (A (A S (A K (A S (A K K)))) 
          (A (A S (A (A S (A K S)) (A (A S (A K K)) I))) (A K I)))))))) 
      (A K (A (A S (A (A S (A K S)) (A (A S (A K (A S (A K S)))) 
        (A (A S (A K (A S (A K K)))) (A (A S (A (A S (A K S)) (A (A S (A K K)) I))) 
          (A K I)))))) (A K (A K I))))))

    example1 :: Run Example1
    example1 = Just (Just (Just (Just (Just 3))))

## Lambda Calculus

This is a limited version of a lambda calculus interpreter, it does not perform alpha conversion.

Note: This is not a type level lambda, because the resulting types must have kind "*". 

Like with the SKI Combinators example, we will not see the result by using ":kind! Example1". We expect that the Example1 will evaluate to "Int" and check it by defining "example1".

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

    type Example1 = App (App (Abs "x" (Abs "y" (Var "x"))) (Con Int)) (Con Bool)

    example1 :: Run Example1
    example1 = 1

Adding numbers 2 and 3 like in the SKI example looks like this:

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

Example3 will evaluate to

	Maybe (Maybe (Maybe (Maybe (Maybe Int))))

## Parser

This is a very simple parser combinator.

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

    type IsDigit = Elem ["1", "2", "3"]
    type IsAlpha = Elem ["a", "b", "c"]
    type Digits = Many IsDigit
    type Alphas = Many IsAlpha

    type Parser1 = Then Alphas (ConsF (Then Digits (ConsF (Return Nil))))

    type Inp1 = '["1", "3"]
    type Inp2 = '["a", "2", "1"]

    type Example1 = Parse Parser1 Inp1 -- fail
    type Example2 = Parse Parser1 Inp2 -- ok

Parser1 expects some alphas and the some digits. Example1 will apply it to an input without the alphas and will fail. Example2 will succeed.

Typing ":kind! Example1" will result in 

	Example1 :: * = Fail

Typing ":kind! Example2" will result in 

	Example2 :: * = Ok '[] (Cons (Cons "a" Nil) (Cons (Cons "2" (Cons "1" Nil)) Nil))
	