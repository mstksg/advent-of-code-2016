-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Solver      ((:~>)(..))
import           Data.List       (transpose, sortBy)
import           Data.List.Split (chunksOf)
import           Text.Read       (readMaybe)

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

