-- Haskell !!!

{- What is Haskell like?
   - Named after Haskell Curry
   - Based on λ Calculus
             ^typed
            ^polymorphic (System F)
   - Rich type system
   - Cancer of the semicolon (lots of syntactic sugar)
     - make Haskell code look like Math
     - get as much as possible out of ASCII
   - Lazy
   - darling language on the block
   - Algebraic Datatypes
 -}

fact :: Integer -> Integer

fact 0 = 1
fact n = n * fact (n-1)

foo x y = 1000 * x * z + 1e-3 * y * z^3
  where
    z = sqrt (x^2 + y^2)


bar x y = 
  let
    z = sqrt (x^2 + y^2)
  in
   1000 * x * z + 1e-3 * y * z^3

-- Algebraic Datatypes merge & generalize: enum, struct, union
-- Constructors for free
-- Accessors via pattern matching

data Foo = Bar Int Char | Baz Bool
         deriving (Show, Eq)

data Booll = Falsee | Truee

trippleFooInt :: Foo -> Foo

trippleFooInt (Bar x c) = Bar (3*x) c
trippleFooInt (Baz b)   = Bar 0 'n'

-- Example of laziness:
-- *Main> trippleFooInt (Baz (fact 1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001 > 2))
-- Bar 0 'n'

-- data ListInt  = Nil | Cons Int ListInt
-- data ListChar = Nil | Cons Int ListChar
data List a = Nil | Cons a (List a)
         deriving (Show, Eq)

-- Built in list syntax:
-- data [a] = [] | a : [a]

-- *Main> Cons 'f' (Cons 'o' (Cons 'o' Nil))
-- Cons 'f' (Cons 'o' (Cons 'o' Nil))
-- *Main> 'f' : ('o' : ('o' : []))
-- "foo"
-- *Main> 1 : (2 : (3 : []))
-- [1,2,3]
-- *Main> ['f','o','o']
-- "foo"

addPairs [] = []
addPairs [x] = []  -- syntactic sugar for:
-- addPairs (x:[]) = []
addPairs (x:(y:ys)) = (x+y):addPairs ys

-- addPairs :: Num a => [a] -> [a]
-- *Main> addPairs [1,2,3,4,40,43,67,9]
-- [3,7,83,76]

-- *Main> :t head
-- head :: [a] -> a
-- *Main> :t tail
-- tail :: [a] -> [a]
-- *Main> :t map
-- map :: (a -> b) -> [a] -> [b]
-- *Main> map sqrt [1,2,3,4,5,6,7,8,9]
-- [1.0,1.4142135623730951,1.7320508075688772,2.0,2.23606797749979,2.449489742783178,2.6457513110645907,2.8284271247461903,3.0]
-- *Main> map sqrt [1..9]
-- [1.0,1.4142135623730951,1.7320508075688772,2.0,2.23606797749979,2.449489742783178,2.6457513110645907,2.8284271247461903,3.0]
