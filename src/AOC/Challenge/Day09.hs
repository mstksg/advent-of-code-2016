{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Prelude

chomp :: String -> Int
chomp = outside 0
  where
    outside n str = case span (/= '(') str of
      (o,[])   -> n + length o
      (o,_:is) -> inside (n + length o) is
    inside n str = case span (/= ')') str of
      (i,[]) -> error "what"
      (i,_:os) -> case span (/= 'x') i of
        (a,_:b) -> outside (n + read a * read b) (drop (read a) os)

day09a :: _ :~> _
day09a = MkSol
    { sParse = Just . filter (not . isSpace)
    , sShow  = show
    , sSolve = Just . chomp
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
