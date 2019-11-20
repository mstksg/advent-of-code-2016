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
  ) where

import           AOC.Prelude hiding     (Context)
import           Crypto.Hash
import           Data.ByteString.Lens
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

coolHash :: Context MD5 -> Int -> Maybe String
coolHash ctx = uncurry dig
             . splitAt 5
             . over packedChars ( B16.encode
                                . BA.convert
                                . hashFinalize
                                . hashUpdate ctx
                                )
             . show
  where
    dig "00000" = Just
    dig _       = const Nothing

day05a :: String :~> String
day05a = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
             . take 8
             . mapMaybe listToMaybe
             . flip mapMaybe [0..]
             . coolHash
             . hashUpdate hashInit
             . view (packedChars @BS.ByteString)
    }

day05b :: String :~> String
day05b = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = findJust (\x -> toList x <$ guard (M.size x == 8))
             . scanl' accumulateMe M.empty
             . flip mapMaybe [0..]
             . coolHash
             . hashUpdate hashInit
             . view (packedChars @BS.ByteString)
    }

accumulateMe :: Map (Finite 8) Char -> String -> Map (Finite 8) Char
accumulateMe mp (x:y:_)
  | Just x' <- readMaybe [x]
  , Just k  <- packFinite x' = M.insertWith (const id) k y mp
accumulateMe mp _       = mp
