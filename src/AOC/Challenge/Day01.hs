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
import           Data.Complex
import           Linear

data S = S { sLoc :: Point, sHeading :: Point }
  deriving Show

data Command = CTurn Point
             | CGo
  deriving Show

stepper :: S -> Command -> S
stepper (S x h) = \case
    CTurn d -> S (x + h') h'
      where
        h' = d `mulPoint` h
    CGo     -> S (x + h) h


day01a :: [Command] :~> Int
day01a = MkSol
    { sParse = parseCmd
    , sShow  = show
    , sSolve = Just . mannDist 0 . sLoc
             . foldl' stepper (S 0 (V2 0 1))
    }

day01b :: [Command] :~> Int
day01b = MkSol
    { sParse = parseCmd
    , sShow  = show
    , sSolve = fmap (mannDist 0)
             . firstRepeated 
             . map sLoc 
             . scanl stepper (S (V2 0 0) (V2 0 1))
    }

parseCmd :: String -> Maybe [Command]
parseCmd str = Just $ do
    x:xs   <- words . filter (/=',') $ str
    h      <- case x of
      'R' -> pure $ V2 0 (-1)
      'L' -> pure $ V2 0 1
      _   -> empty
    Just n <- pure $ readMaybe xs
    CTurn h : replicate (n - 1) CGo

