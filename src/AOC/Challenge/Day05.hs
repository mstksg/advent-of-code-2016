{-# LANGUAGE NumericUnderscores       #-}
{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
  -- , searchBlockPar
  ) where

import           AOC.Prelude hiding     (Context)
import           Crypto.Hash
import           Data.ByteString.Lens
import           Control.Parallel.Strategies
import           Data.Finite
import           Data.Word
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Finitary          as F
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

coolHash :: Context MD5 -> Int -> Maybe (Finite 16, Finite 16)
coolHash ctx i = case concatMap (tupleToList . splitWord) (BS.unpack hashed) of
    0:0:0:0:0:x:y:_ -> Just (x, y)
    _               -> Nothing
  where
    hashed = BA.convert . hashFinalize . hashUpdate ctx
           . view (packedChars @BS.ByteString)
           $ show i

day05a :: Context MD5 :~> [Finite 16]
day05a = MkSol
    { sParse = Just . hashUpdate hashInit . view (packedChars @BS.ByteString)
    , sShow  = map (review hexDigit)
    , sSolve = \ctx -> Just
                     . take 8
                     . (foldMap . foldMapParChunk 500_000)
                        (maybeToList . fmap fst . coolHash ctx)
                     $ chunksOf 10_000_000 [0..]
    }

coolHash2 :: Context MD5 -> Int -> Maybe (Finite 8, Finite 16)
coolHash2 ctx i = do
    (x, y) <- coolHash ctx i
    k      <- strengthenN x
    pure (k, y)

day05b :: Context MD5 :~> Map (Finite 8) (Finite 16)
day05b = MkSol
    { sParse = Just . hashUpdate hashInit . view (packedChars @BS.ByteString)
    , sShow  = map (review hexDigit) . toList
    , sSolve = \ctx -> find ((== 8) . M.size)
                     . scanl' (\mp (k, x) -> M.insertWith (const id) k x mp) M.empty
                     . (foldMap . foldMapParChunk 500_000)
                          (maybeToList . coolHash2 ctx)
                     $ chunksOf 10_000_000 [0..]
    }
