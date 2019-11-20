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


data Turtle = (:@) { tLoc :: Point
                   , tDir ::  Dir
                   }
  deriving Show

data Command = CTurn Dir
             | CGo
  deriving Show

stepper :: Turtle -> Command -> Turtle
stepper (x :@ h) = \case
    CTurn d -> (x + dirPoint h') :@ h'
      where
        h' = d <> h
    CGo     -> (x + dirPoint h ) :@ h


day01a :: [Command] :~> Int
day01a = MkSol
    { sParse = parseCmd
    , sShow  = show
    , sSolve = Just . mannDist 0 . tLoc
             . foldl' stepper (0 :@ North)
    }

day01b :: [Command] :~> Int
day01b = MkSol
    { sParse = parseCmd
    , sShow  = show
    , sSolve = fmap (mannDist 0)
             . firstRepeated 
             . map tLoc 
             . scanl stepper (0 :@ North)
    }

parseCmd :: String -> Maybe [Command]
parseCmd str = Just $ do
    x:xs   <- words . filter (/=',') $ str
    Just h <- pure $ parseDir x
    Just n <- pure $ readMaybe xs
    CTurn h : replicate (n - 1) CGo

