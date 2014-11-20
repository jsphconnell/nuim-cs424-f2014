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
 -}

fact n =
  if n==0 then 1 else n * fact (n-1)
