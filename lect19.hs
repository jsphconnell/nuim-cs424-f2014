-- More monads

import Control.Monad

getAndPutChar = getChar >>= putChar
getAndPutChar' = getChar >>= putChar >>= (\_ -> putChar '\n')

{-
getAndPutChar'' =
  getChar
  >>= (\c -> putChar c
             >>= \_ -> putChar c
                       >>= (\_ -> putChar '\n'))
-}

-- (>>) is defined thus:
-- m1 >> m2 = m1 >>= (\_ -> m2)

-- Can use to define instead like this:

getAndPutChar'' =
  getChar >>= (\c -> putChar c >> putChar c >> putChar '\n')

printString :: [Char] -> IO ()
printString [] = putChar '\n'
printString (c:cs) = putChar c >> printString cs

-- *Main> foldl (+) 0 [1,2,3,4,5]
-- 15
-- *Main> foldl (*) 1 [1,2,3,4,5]
-- 120
-- *Main> foldl (*) 1 [1..10]
-- 3628800

printString' :: [Char] -> IO ()
printString' cs = foldl
                  (>>)
                  (return ())
                  (map putChar cs ++ [putChar '\n'])

repeatIO :: Int -> IO a -> IO ()
repeatIO 0 io = return ()
repeatIO n io = io >> repeatIO (n-1) io

repeatIO' :: Int -> IO a -> IO a
repeatIO' n io = foldl (>>) (return undefined) (take n (repeat io))

-- Find some Pythagorean Triples,
-- i.e., (x,y,z) such that x^2+y^2=z^2

pyths n =
  map (\x->
        map (\y -> 
              map (\z -> 
                    if x^2+y^2==z^2
                    then [(x,y,z)]
                    else [])
              [1..n])
        [1..n])
  [1..n]

pyths' n =
  concat (map (\x->
                concat (map (\y -> 
                              concat (map (\z -> 
                                            if x^2+y^2==z^2
                                            then [(x,y,z)]
                                            else [])
                                      [1..n])) 
                        [1..n])) 
          [1..n])

-- concat xs = foldl (++) [] xs

pyths'' n =
  concatMap (\x->
              concatMap (\y -> 
                          concatMap (\z -> 
                                      if x^2+y^2==z^2
                                      then [(x,y,z)]
                                      else [])
                          [1..n])
              [1..n])
  [1..n]

pyths''' n =
  [1..n] 
  >>= (\x -> 
        [1..n] 
          >>= (\y -> 
                [1..n] 
                >>= (\z -> 
                      if x^2+y^2==z^2
                      then [(x,y,z)]
                      else [])))

-- syntactic sugar: "do" syntax for monads
pyths'''' n = do
  x <- [1..n]
  y <- [1..n]
  z <- [1..n]
  if x^2+y^2==z^2
    then return (x,y,z)
    else mzero

printSomeGoo = do
  x <- printString "foo"        -- printString "foo" >>= (\x -> ...
  y <- printString "bar"
  z <- printString "baz"
  return ()
  
printSomeGoo' = do
  printString "foo"             -- printString "foo" >>
  printString "bar"
  printString "baz"
  return ()
