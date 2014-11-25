-- More Haskell!

-- "infix thing" like + or < becomes "regular" thing by surrounding
-- with parens, (+) or (<).

-- Super slow Fibonacci Numbers:
-- fib1 :: Int -> Integer
-- fib1 :: Int -> Double
fib1 :: Num a => Int -> a
fib1 0 = 1
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

-- Faster Fibonacci Numbers via Tail Recursion:
fib2 0 = 1
fib2 n = fibTail 1 1 1 n
  where
    -- fibTail returns (fib n), but
    -- fi must be (fib i), fim1 must be (fib (i-1)), i<=n, 
    fibTail i fi fim1 n | i<n = fibTail (i+1) (fi+fim1) fi n
    fibTail i fi fim1 n | i==n = fi

fibs1 = map fib2 [0..]

fibs2 = 1:1:(zipWith (+) fibs2 (tail fibs2))

{-
Prelude> zipWith max [0,10,2,30,4] [3,4,5,90,1]
[3,10,5,90,4]

Prelude> zipWith + [0,10,2,30,4] [3,4,5,90,1]
HORRIBLE TYPE ERROR!!!!

Prelude> zipWith (+) [0,10,2,30,4] [3,4,5,90,1]
[3,14,7,120,5]

  2 + 3
   is sugar for 
  (+) 2 3

  (*2)
   is sugar for
  \x->x*2
  which means the same as the Scheme: (lambda (x) (* x 2))     

"Slices":

Prelude> (2/) 10
0.2
Prelude> (/2) 10
5.0

Prelude> 13 `mod` 10
3
Prelude> mod 13 10
3

type String is [Char], aka list of characters

*Main> length "foo"
3
*Main> :t length
length :: [a] -> Int

*Main> words "the quick brown fox jumps over the lazy dog."
["the","quick","brown","fox","jumps","over","the","lazy","dog."]

This is efficient, even when enormousString is contents of enormous file
*Main> words enormousString !! 7

-}

data Mebe = Joost Double | Nada
          deriving Show
addMebe :: Mebe -> Mebe -> Mebe
addMebe (Joost x) (Joost y) = Joost (x+y)
addMebe Nada _ = Nada           -- _ is "unused variable don't warn me"
addMebe _ Nada = Nada

liftMebe f (Joost x) (Joost y) = Joost (f x y)
liftMebe f Nada _ = Nada
liftMebe f _ Nada = Nada

-- Could define
addMebe1 = liftMebe (+)

{-
*Main> Joost 2 `addMebe` Joost 3
Joost 5.0
*Main> Joost 2 `addMebe` Nada
Nada
*Main> Nada `addMebe` Joost 3
Nada
*Main> Nada `addMebe` Nada
Nada
-}
