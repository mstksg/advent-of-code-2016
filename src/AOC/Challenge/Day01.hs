{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day01 (
    day01a
  -- , day01b
  ) where

import           AOC.Prelude
import           Data.Complex
import           Linear

type Heading = Complex Int
type Point   = V2 Int

data S = S { sLoc :: Point, sHeading :: Heading }

type Command = (Heading, Int)

day01a :: [Command] :~> Int
day01a = MkSol
    { sParse = Just
             . map (\(x:xs) -> (parseHead x, read xs))
             . words
             . filter (/= ',')
    , sShow  = show
    , sSolve = undefined
    }


parseHead 'R' = 0 :+ (-1)
parseHead 'L' = 0 :+ 1

day01b :: _ :~> _
day01b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
