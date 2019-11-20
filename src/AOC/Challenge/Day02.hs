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
  , constrain
  , pointNum
  , day02b
  ) where

import           AOC.Prelude
import           Linear
import qualified Data.Map    as M

day02a :: [[Point]] :~> [Int]
day02a = MkSol
    { sParse = Just . map (mapMaybe (`M.lookup` dirMap)) . lines
    , sShow  = map intToDigit
    , sSolve = sequence . snd . mapAccumL stepper 0
    }

stepper x = (\r -> (r, M.lookup r pointNum))
          . foldl' (\y -> constrain . (+ y)) x
                         

constrain :: Point -> Point
constrain = liftA2 min (V2 1 1) . liftA2 max (V2 (-1) (-1))

pointNum = M.fromList . flip zip [1..] $
    [ V2 (-1) 1
    , V2    0 1
    , V2    1 1
    , V2 (-1) 0
    , V2    0 0
    , V2    1 0
    , V2 (-1) (-1)
    , V2    0 (-1)
    , V2    1 (-1)
    ]

dirMap = M.fromList [('U',V2 0 1), ('D',V2 0 (-1)), ('R',V2 1 0), ('L', V2 (-1) 0)]

day02b :: _ :~> _
day02b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
