{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Prelude
import           Data.Map    (Map)
import           Linear
import qualified Data.Map    as M

stepper :: Map Point Char -> Point -> [Dir] -> (Point, Maybe Char)
stepper mp x = (\r -> (r, M.lookup r mp))
             . foldl' move x
  where
    move p d
        | p' `M.member` mp = p'
        | otherwise        = p
      where
        p' = p + dirPoint d

keypadA :: Map Point Char
keypadA = M.fromList . flip zip ['1'..] $
    [ V2 (-1)   1
    , V2   0    1
    , V2   1    1
    , V2 (-1)   0
    , V2   0    0
    , V2   1    0
    , V2 (-1) (-1)
    , V2   0  (-1)
    , V2   1  (-1)
    ]

day02a :: [[Dir]] :~> String
day02a = MkSol
    { sParse = Just . (map . mapMaybe) parseDir . lines
    , sShow  = id
    , sSolve = sequence . snd . mapAccumL (stepper keypadA) 0
    }

keypadB :: Map Point Char
keypadB = M.fromList . flip zip (['1'..'9'] ++ ['A'..]) $
    [ V2   0    2
    , V2 (-1)   1
    , V2   0    1
    , V2   1    1
    , V2 (-2)   0
    , V2 (-1)   0
    , V2   0    0
    , V2   1    0
    , V2   2    0
    , V2 (-1) (-1)
    , V2   0  (-1)
    , V2   1  (-1)
    , V2   0  (-2)
    ]

day02b :: [[Dir]] :~> String
day02b = MkSol
    { sParse = Just . (map . mapMaybe) parseDir . lines
    , sShow  = id
    , sSolve = sequence . snd . mapAccumL (stepper keypadB) (V2 (-2) 0)
    }

