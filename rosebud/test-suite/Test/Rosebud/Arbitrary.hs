-- Copyright (c)2012, Antoine Latter
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Antoine Latter nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- Instances adapted from the @quickcheck-instances@ package:
--
-- https://github.com/haskellari/qc-instances/blob/52f1105b57988c495d6fc5eaa220e394b06af8e9/src/Test/QuickCheck/Instances/Containers.hs

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Rosebud.Arbitrary () where

import Control.Applicative (Applicative(liftA2))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Maybe (mapMaybe)
import Test.QuickCheck
  ( Arbitrary(arbitrary, shrink), Arbitrary1(liftArbitrary, liftShrink), Gen, arbitrary1, choose
  , liftShrink2, shrink1, shuffle, sized
  )
import qualified Data.Tree as Tree

instance Arbitrary1 Tree.Tree where
  liftArbitrary arb = sized $ \n -> do
    k <- choose (0, n)
    go k
    where
    go n = do -- n is the size of the trees.
      value <- arb
      pars <- arbPartition (n - 1) -- can go negative!
      forest <- traverse go pars
      return $ Tree.Node value forest

    arbPartition :: Int -> Gen [Int]
    arbPartition k = case compare k 1 of
      LT -> pure []
      EQ -> pure [1]
      GT -> do
        first <- choose (1, k)
        rest <- arbPartition $ k - first
        shuffle (first : rest)

  liftShrink shr = go
    where
    go (Tree.Node val forest) = forest ++
      [ Tree.Node e fs
      | (e, fs) <- liftShrink2 shr (liftShrink go) (val, forest)
      ]

instance Arbitrary a => Arbitrary (Tree.Tree a) where
  arbitrary = arbitrary1
  shrink = shrink1

-------------------------------------------------------------------------------

-- Instances adapted from the @quickcheck-instances@ package:
--
-- https://github.com/haskellari/qc-instances/blob/6a4509252380296c66f3193ff89c5aa9f5f6b4d6/src/Test/QuickCheck/Instances/Semigroup.hs

instance Arbitrary1 NonEmpty where
  liftArbitrary arb = liftA2 (:|) arb (liftArbitrary arb)
  liftShrink shr (x :| xs) = mapMaybe nonEmpty . liftShrink shr $ x : xs

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = arbitrary1
  shrink = shrink1
