-- Continue with
--   Mebe (aka Maybe)
--   Joost (aka Just)
--   Nada (aka Nothing)

data Mebe = Joost Double | Nada
          deriving Show

liftMebe2 :: (Double -> Double -> Double) -> Mebe -> Mebe -> Mebe
liftMebe2 f (Joost x) (Joost y) = Joost (f x y)
liftMebe2 f Nada _ = Nada
liftMebe2 f _ Nada = Nada

-- Could define
-- addMebe x y = liftMebe2 (+) x y
-- addMebe x = liftMebe2 (+) x -- eta reduce prev line
addMebe = liftMebe2 (+) -- eta reduce prev line

{-
*Main> Joost 2 `addMebe` Joost 3
Joost 5.0
*Main> Joost 2 `addMebe` Nada
Nada
*Main> Nada `addMebe` Joost 3
Nada
*Main> Nada `addMebe` Nada
Nada

*Main> liftMebe (*) (Joost 2) (Joost 3)
Joost 6.0
-}

-- version of "reciprocal" for Mebe:

recipMebe Nada = Nada
recipMebe (Joost 0) = Nada
recipMebe (Joost x) = Joost (1 / x)

liftMebe1 :: (Double -> Double) -> Mebe -> Mebe
liftMebe1 f Nada = Nada
liftMebe1 f (Joost x) = Joost (f x)

--  recipMebe
-- is not
--  liftMebe1 (1/)
-- because they disagree on
--  recipMebe (Joost 0)

chainToMebe :: (Double -> Mebe) -> Mebe -> Mebe
chainToMebe f Nada = Nada
chainToMebe f (Joost x) = f x

-- Note: can use ' in an identifier, like foo'bar''baz''' = 7
recipMebe' = chainToMebe (\x -> if x==0 then Nada else Joost (1/x))
