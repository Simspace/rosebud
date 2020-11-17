{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Test.RosebudSpec
  ( spec
  ) where

import Data.Tree (Tree(Node, subForest))
import Test.Hspec
import qualified Data.Foldable as Foldable
import qualified Data.Tree as Tree
import qualified Rosebud
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Instances.Containers ()

spec :: Spec
spec = parallel do
  describe "sortTree" do
    it "tree size stays the same" do
      QC.property @(Tree Int -> Bool) \x ->
        Foldable.length (Rosebud.sortTree x) == Foldable.length x
    it "children are sorted" do
      QC.property @(Tree Int -> Bool) \x ->
        childrenSortedBy (<=) $ Rosebud.sortTree x

  describe "sortTreeOn" do
    it "tree size stays the same" do
      QC.property @(Tree Int -> Bool) \x ->
        Foldable.length (Rosebud.sortTreeOn negate x) == Foldable.length x
    it "children are sorted" do
      QC.property @(Tree Int -> Bool) \x ->
        childrenSortedBy (>=) $ Rosebud.sortTreeOn negate x

  describe "sortForest" do
    it "forest size stays the same" do
      QC.property @([Tree Int] -> Bool) \x ->
        sum (fmap (Foldable.length) (Rosebud.sortForest x))
          == sum (fmap Foldable.length x)
    it "forest is sorted by the root nodes" do
      QC.property @([Tree Int] -> Bool) \x ->
         isSortedBy (<=) $ fmap Tree.rootLabel $ Rosebud.sortForest x
    it "each tree's children are sorted" do
      QC.property @([Tree Int] -> Bool) \x ->
        and $ fmap (childrenSortedBy (<=)) $ Rosebud.sortForest x

  describe "sortForestOn" do
    it "forest size stays the same" do
      QC.property @([Tree Int] -> Bool) \x ->
        sum (fmap (Foldable.length) (Rosebud.sortForestOn negate x))
          == sum (fmap Foldable.length x)
    it "forest is sorted by the root nodes" do
      QC.property @([Tree Int] -> Bool) \x ->
         isSortedBy (>=) $ fmap Tree.rootLabel $ Rosebud.sortForestOn negate x
    it "each tree's children are sorted" do
      QC.property @([Tree Int] -> Bool) \x ->
        and $ fmap (childrenSortedBy (>=)) $ Rosebud.sortForestOn negate x

--  describe "sortNEForest" do
--    it "works" do
--      pending
--
--  describe "sortNEForestOn" do
--    it "works" do
--      pending
--
--  describe "findNodeInTree" do
--    it "works" do
--      pending
--
--  describe "isSubtreeOf" do
--    it "works" do
--      pending
--
--  describe "isExactSubtreeOf" do
--    it "works" do
--      pending
--
--  describe "isSubtreeOfBy" do
--    it "works" do
--      pending
--
--  describe "findNodeInForest" do
--    it "works" do
--      pending
--
--  describe "enumerateTree" do
--    it "works" do
--      pending
--
--  describe "zipTree" do
--    it "works" do
--      pending
--
--  describe "zipWithTree" do
--    it "works" do
--      pending
--
--  describe "enumerateForest" do
--    it "works" do
--      pending
--
--  describe "enumerateNEForest" do
--    it "works" do
--      pending
--
--  describe "mapForest" do
--    it "works" do
--      pending
--
--  describe "mapNEForest" do
--    it "works" do
--      pending
--
--  describe "zipForest" do
--    it "works" do
--      pending
--
--  describe "zipNEForest" do
--    it "works" do
--      pending
--
--  describe "zipWithForest" do
--    it "works" do
--      pending
--
--  describe "zipWithNEForest" do
--    it "works" do
--      pending
--
--  describe "flattenForest" do
--    it "works" do
--      pending
--
--  describe "flattenNEForest" do
--    it "works" do
--      pending
--
--  describe "singletonTree" do
--    it "works" do
--      pending
--
--  describe "indicesTree" do
--    it "works" do
--      pending
--
--  describe "eitherTreeFromLabels" do
--    it "works" do
--      pending
--
--  describe "unsafeTreeFromLabels" do
--    it "works" do
--      pending
--
--  describe "singletonForest" do
--    it "works" do
--      pending
--
--  describe "singletonNEForest" do
--    it "works" do
--      pending
--
--  describe "indicesForest" do
--    it "works" do
--      pending
--
--  describe "indicesNEForest" do
--    it "works" do
--      pending
--
--  describe "subtrees" do
--    it "works" do
--      pending
--
--  describe "neSubtrees" do
--    it "works" do
--      pending
--
--  describe "eitherNEForestFromLabels" do
--    it "works" do
--      pending
--
--  describe "unsafeNEForestFromLabels" do
--    it "works" do
--      pending
--
--  describe "neForest" do
--    it "works" do
--      pending
--
--  describe "unsafeNEForest" do
--    it "works" do
--      pending

childrenSortedBy :: (a -> a -> Bool) -> Tree a -> Bool
childrenSortedBy f = \case
  Node { subForest } -> and $ b : bs where
    b = isSortedBy f $ fmap Tree.rootLabel subForest
    bs = fmap (childrenSortedBy f) subForest

isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy f xs = and $ zipWith f xs $ drop 1 xs
