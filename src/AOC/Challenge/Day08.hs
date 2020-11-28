-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Solver      ((:~>)(..))
import           Advent.OCR      (parseLettersWith)
import           Data.Finite     (Finite, packFinite, modulo, unshift)
import           Data.List       (foldl')
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import           Text.Read       (readMaybe)
import qualified Data.Set        as S

type Screen = Set (Finite 50, Finite 6)

day08a :: [Command] :~> Int
day08a = MkSol
    { sParse = traverse parseCommand . lines
    , sShow  = show
    , sSolve = Just . S.size . foldl' (flip runCommand) S.empty
    }

day08b :: [Command] :~> Screen
day08b = MkSol
    { sParse = traverse parseCommand . lines
    , sShow  = fromMaybe ""
             . parseLettersWith (fromIntegral . fst) (fromIntegral . snd)
    , sSolve = Just . foldl' (flip runCommand) S.empty
    }

data Command = Rect   (Finite 51) (Finite 7 )
             | RotRow (Finite 6 ) (Finite 50)
             | RotCol (Finite 50) (Finite 6 )
  deriving Show

runCommand :: Command -> Screen -> Screen
runCommand = \case
    Rect x y   ->
      let x' = fromMaybe 0 $ unshift x
          y' = fromMaybe 0 $ unshift y
      in  S.union $ S.fromList ((,) <$> [0..x'] <*> [0..y'])
    RotRow r n -> S.map (\(c,r') -> if r' == r then (c+n, r') else (c, r'))
    RotCol c n -> S.map (\(c',r) -> if c' == c then (c', r+n) else (c', r))

parseCommand :: String -> Maybe Command
parseCommand str = case words str of
  "rect":s:_ -> do
    x:y:_ <- Just $ splitOn "x" s
    Rect <$> (packFinite =<< readMaybe x) <*> (packFinite =<< readMaybe y)
  "rotate":"row":(_:_:r):_:n:_ -> do
    RotRow <$> (packFinite =<< readMaybe r) <*> (modulo <$> readMaybe n)
  "rotate":"column":(_:_:c):_:n:_ -> do
    RotCol <$> (packFinite =<< readMaybe c) <*> (modulo <$> readMaybe n)
  _ -> Nothing
