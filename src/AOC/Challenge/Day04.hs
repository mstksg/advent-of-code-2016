{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day04 where

-- module AOC.Challenge.Day04 (
--     day04a
--   , day04b
--   ) where

import           AOC.Prelude
import           Data.Finite
import           Data.List.Split
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Data.Text       as T

data Room = Room { rName  :: [String]
                 , rId    :: Int
                 }
  deriving (Show)

parseRoom :: String -> Maybe Room
parseRoom (reverse.splitOneOf"-[]"->(_:c:n:rs)) =
    Room (reverse rs) (read n) <$ guard validRoom
  where
    validRoom = all (uncurry (==))
              . zip c
              . map snd
              . freqList
              . concat
              $ rs
parseRoom _ = Nothing

day04a :: _ :~> _
day04a = MkSol
    { sParse = Just . mapMaybe parseRoom . lines
    , sShow  = show
    , sSolve = Just . sum . map rId
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = Just . mapMaybe parseRoom . lines
    , sShow  = show
    , sSolve = firstJust $ \(Room n i) -> i <$ do
        guard $ "north" `isInfixOf` (concatMap . map) (caeser (modulo (fromIntegral i))) n
    }
