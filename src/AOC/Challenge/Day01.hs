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
  , day01b
  ) where

import           AOC.Prelude
import           AOC.Common.Point
import           Linear ((*^))

getDir :: [Char] -> Maybe (Dir, Int)
getDir (x:xs) = (,) <$> parseDir x <*> readMaybe xs
getDir [] = Nothing

proceed :: (Dir, Point) -> (Dir, Int) -> ((Dir, Point), [Point])
proceed (currDir, pt) (turn, dist) =
    ((newDir, newPoint), take dist (iterate (+ dirPoint newDir) pt))
  where
    newDir = turn <> currDir
    newPoint = pt + (dist *^ dirPoint newDir)

day01a :: [(Dir, Int)] :~> Point
day01a = MkSol
    { sParse = traverse (getDir . strip) . splitOn ","
    , sShow  = show . mannDist 0
    , sSolve = Just . snd . foldl' (\s -> fst . proceed s) (North, 0)
    }

day01b :: [(Dir, Int)] :~> Point
day01b = MkSol
    { sParse = traverse (getDir . strip) . splitOn ","
    , sShow  = show . mannDist 0
    , sSolve = firstRepeated . concat . snd . mapAccumL proceed (North, 0)
    }
