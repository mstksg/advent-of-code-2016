{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC.Prelude
import qualified Data.Set as S

day07a :: [([String], [String])] :~> Int
day07a = MkSol
    { sParse = Just . map chunkUp . lines
    , sShow  = show
    , sSolve = Just . length . filter (uncurry validIP)
    }
  where
    validIP outer inner = not (null (concatMap findAbba outer))
                       && null (concatMap findAbba inner)

day07b :: [([String], [String])] :~> Int
day07b = MkSol
    { sParse = Just . map chunkUp . lines
    , sShow  = show
    , sSolve = Just . length . filter (uncurry validIP)
    }
  where
    validIP outer inner = not (S.null (S.intersection abaO abaI))
      where
        abaO = S.fromList (concatMap findAba outer)
        abaI = S.fromList (swap <$> concatMap findAba inner)

findAbba :: String -> [(Char, Char)]
findAbba (a:b:c:d:xs)
  | a /= b && a == d && b == c = (a,b) : findAbba (b:c:d:xs)
  | otherwise                  = findAbba (b:c:d:xs)
findAbba _ = []

findAba :: String -> [(Char, Char)]
findAba (a:b:c:xs)
  | a == c && a /= b = (a,b) : findAba (b:c:xs)
  | otherwise        = findAba (b:c:xs)
findAba _ = []

chunkUp :: String -> ([String], [String])
chunkUp = outOfBracket
  where
    outOfBracket str = case span (/= '[') str of
      (xs, []  ) -> ([xs], [])
      (xs, _:ys) -> first (xs:) (inBracket ys)
    inBracket str = case span (/= ']') str of
      (xs, []  ) -> ([], [xs])
      (xs, _:ys) -> second (xs:) (outOfBracket ys)
