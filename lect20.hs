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
interleave (x:xs) ys = x:interleave ys xs

interleaveToInfinity :: [[a]] -> [a]
interleaveToInfinity xss = foldr interleave [] xss

-- *Main> take 100 $ interleaveToInfinity [[10^i..]|i<-[0..]]
-- [1,10,2,100,3,11,4,1000,5,12,6,101,7,13,8,10000,9,14,10,102,11,15,12,1001,13,16,14,103,15,17,16,100000,17,18,18,104,19,19,20,1002,21,20,22,105,23,21,24,10001,25,22,26,106,27,23,28,1003,29,24,30,107,31,25,32,1000000,33,26,34,108,35,27,36,1004,37,28,38,109,39,29,40,10002,41,30,42,110,43,31,44,1005,45,32,46,111,47,33,48,100001,49,34,50,112]
