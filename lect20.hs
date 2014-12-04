-- Haskell, List Comprehensions

import Control.Monad

-- Make Haskell look like math.

-- { (x,y,z) | x in {1,...,9},
--             y in {1,...,9}, 
--             z in {1,...,9}, 
--             x^2+y^2=z^2 }

pyths n = [ (x,y,z) |
            x <- [1..n],
            y <- [1..n],
            x <= y,
            z <- [1..n],
            x^2 + y^2 == z^2 ]

-- Sytax of "list comprehension"
-- [ elementExpression | comma-separated-clauses ]
-- clause ::= var <- list
--            boolean condition
-- elementExpression can refer to all variables defined in clauses.
-- each clause can refer to variables defined in earlier clauses.

pyths' n = [ (x,y,z) |
             x <- [1..n],
             y <- [x..n],
             z <- [1..n],
             x^2 + y^2 == z^2 ]

-- pyths in imperitive style:

-- pyths n =
--  for x in [1..n]
--    for y in [1..n]
--      if not (x<=y) then break
--      for z in [1..n]
--        if not (x^2 + y^2 == z^2) then break
--        accumulate (x,y,z)

-- pyths using list monad:

pythsLM :: Int -> [(Int,Int,Int)]
pythsLM n = do
  x <- [1..n]
  y <- [1..n]
  _ <- if x <= y then [()] else []
  z <- [1..n]
  _ <- if (x^2 + y^2 == z^2) then [()] else []
  return (x,y,z)

pythsLM' :: Int -> [(Int,Int,Int)]
pythsLM' n = do
  x <- [1..n]
  y <- [1..n]
  if x <= y then [()] else []
  z <- [1..n]
  if (x^2 + y^2 == z^2) then [()] else []
  return (x,y,z)

pythsLM'' :: Int -> [(Int,Int,Int)]
pythsLM'' n = do
  x <- [1..n]
  y <- [1..n]
  guard (x <= y)
  z <- [1..n]
  guard (x^2 + y^2 == z^2)
  return (x,y,z)

guard' True = return ()
guard' False = []

pythsLM''' :: Int -> [(Int,Int,Int)]
pythsLM''' n =
  [1..n] >>= (\x -> [1..n]
                    >>= (\y -> guard' (x <= y)
                               >> [1..n]
                               >>= (\z -> guard' (x^2 + y^2 == z^2)
                                          >> return (x,y,z))))

-- Example of list monad

-- Prelude> [1,4,6] >>= (\x -> take x (repeat (x^2)))
-- [1,16,16,16,16,36,36,36,36,36,36]

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) ys = x:interleave ys xs

interleaveToInfinity :: [[a]] -> [a]
interleaveToInfinity xss = foldl interleave [] xss

-- *Main> take 100 $ interleaveToInfinity [[n,2*n..] | n <- [1..]]
-- ^CInterrupted.
