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
