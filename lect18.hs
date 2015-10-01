-- Continue with
--   Mebe (aka Maybe)
--   Joost (aka Just)
--   Nada (aka Nothing)

data Mebe a = Joost a | Nada
          deriving Show

liftMebe2 :: (a -> b -> c) -> Mebe a -> Mebe b -> Mebe c
liftMebe2 f (Joost x) (Joost y) = Joost (f x y)
liftMebe2 f Nada _ = Nada
liftMebe2 f _ Nada = Nada

-- Could define
-- addMebe x y = liftMebe2 (+) x y
-- addMebe x = liftMebe2 (+) x -- eta reduce prev line
addMebe = liftMebe2 (+) -- eta reduce prev line

-- version of "reciprocal" for Mebe:

liftMebe1 :: (a -> b) -> Mebe a -> Mebe b
liftMebe1 f Nada = Nada
liftMebe1 f (Joost x) = Joost (f x)

--  recipMebe
-- is not
--  liftMebe1 (1/)
-- because they disagree on
--  recipMebe (Joost 0)

chainToMebe :: (a -> Mebe b) -> Mebe a -> Mebe b
chainToMebe f Nada = Nada
chainToMebe f (Joost x) = f x

-- Note: can use ' in an identifier, like foo'bar''baz''' = 7
recipMebe' = chainToMebe (\x -> if x==0 then Nada else Joost (1/x))

-- In Haskell (standard Prelude)
--   type, Maybe a
--   Just :: a -> Maybe a
--   Nothing :: Maybe a

-- chainToMebe is called: (>>=) aka "bind"

-- *Main> Just 2 >>= (\x -> if x==0 then Nothing else Just (1/x))
-- Just 0.5
-- *Main> Nothing >>= (\x -> if x==0 then Nothing else Just (1/x))
-- Nothing

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

-- (>>=) :: Possible a -> (a -> Possible b) -> Posible b
-- *Main> [1,2,3] >>= (\x -> [5*x, 7*x])
-- [5,7,10,14,15,21]
-- where "Possible a" is [a], list of a's.

-- return :: x -> Maybe x
-- returnMaybe x = Just x

-- return :: x -> [x]
-- returnPossible x = [x]

-- In Prelude,
-- return :: Monad m => a -> m a
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Defer...
-- Monad Axioms:
--   return x >>= f    =    f x

-- outputCharacter :: Char -> (World -> (World, ()))
-- inputCharacter  ::          World -> (World, Char)
-- openFile        :: String -> (World -> (World, ()))

-- data IO a = World -> (World, a)

-- *Main> :t getChar 
-- getChar :: IO Char
-- *Main> :t putChar
-- putChar :: Char -> IO ()
-- *Main> :t getChar >>= putChar
-- getChar >>= putChar :: IO ()
getAndPutChar = getChar >>= putChar
getAndPutChar' = getChar >>= putChar >>= (\_ -> putChar '\n')
getAndPutChar'' =
  getChar
  >>= (\c -> putChar c
             >>= \_ -> putChar c
                       >>= (\_ -> putChar '\n'))
