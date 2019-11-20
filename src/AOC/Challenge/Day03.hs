{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude
import           Data.List.Split

isTriangle :: [Int] -> Bool
isTriangle (sortBy (flip compare)->(x:xs)) = sum xs > x
isTriangle _ = False

day03a :: [[Int]] :~> Int
day03a = MkSol
    { sParse = traverse (traverse readMaybe . words) . lines
    , sShow  = show
    , sSolve = Just . length . filter isTriangle
    }

day03b :: [[Int]] :~> _
day03b = MkSol
    { sParse = traverse (traverse readMaybe . words) . lines
    , sShow  = show
    , sSolve = Just
             . length
             . filter isTriangle
             . concatMap transpose
             . chunksOf 3
    }

