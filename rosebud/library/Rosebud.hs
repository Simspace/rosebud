{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rosebud
  ( -- * Introduction
    -- $intro

    -- * Re-exports
    Tree(Node, rootLabel, subForest)
  , Forest

    -- * Types
  , NEForest

    -- * Sorting
    -- ** Trees
  , sortTree
  , sortTreeOn
    -- ** Forests
  , sortForest
  , sortForestOn
  , sortNEForest
  , sortNEForestOn

    -- * Searching
    -- ** Trees
  , findNodeInTree
  , isSubtreeOf
  , isExactSubtreeOf
  , isSubtreeOfBy
    -- ** Forests
  , findNodeInForest
  , isSubtreeIn
  , isExactSubtreeIn
  , isSubtreeInBy

    -- * Transformation
    -- ** Trees
  , enumerateTree
    -- ** Forests
  , enumerateForest
  , enumerateNEForest
  , mapForest
  , mapNEForest
  , zipForest
  , zipNEForest
  , zipWithForest
  , zipWithNEForest
  , flattenForest
  , flattenNEForest

    -- * Construction
    -- ** Trees
  , singletonTree
  , indicesTree
  , eitherTreeFromLabels
  , unsafeTreeFromLabels
    -- ** Forests
  , singletonForest
  , singletonNEForest
  , indicesForest
  , indicesNEForest
  , subtrees
  , neSubtrees
  , eitherNEForestFromLabels
  , unsafeNEForestFromLabels
  , neForest
  , unsafeNEForest

    -- ** Errors
  , NEForestFromLabelsError(NoRootsFoundError, OrphansFoundError)
  , TreeFromLabelsError(TooManyTreesError, ForestFromLabelsError)
  ) where

import Control.Exception (Exception)
import Control.Monad.Trans.State (State)
import Data.List.NonEmpty (NonEmpty((:|)), NonEmpty)
import Data.Monoid (Alt(Alt))
import Data.Tree (Tree(Node, rootLabel, subForest), Forest)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prelude
import qualified Control.Exception as Ex
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Zip as Zip
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord
import qualified Data.Semigroup as Semigroup
import qualified Data.Tree as Tree

-- | A convenience type alias for a non-empty 'Forest'.
--
-- @since 0.1.0.0
type NEForest a = NonEmpty (Tree a)

-- | The error type for 'eitherNEForestFromLabels'.
--
-- @since 0.1.0.0
data NEForestFromLabelsError a
  = -- | No root label(s) were found. Provides the flat list of input labels.
    --
    -- @since 0.1.0.0
    NoRootsFoundError (NonEmpty a)
    -- | Orphan labels were found. Provides the assembled 'Forest' and a flat
    -- list of orphan labels.
    --
    -- @since 0.1.0.0
  | OrphansFoundError (Forest a) (NonEmpty a)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | The error type for 'eitherTreeFromLabels'.
--
-- @since 0.1.0.0
data TreeFromLabelsError a
  = -- | Produced more than one 'Tree' when only one 'Tree' was expected.
    -- Provides back all assembled 'Tree' values in an 'NEForest'.
    --
    -- @since 0.1.0.0
    TooManyTreesError (NEForest a)
    -- | Produced via the internal call to 'eitherNEForestFromLabels'.
    --
    -- @since 0.1.0.0
  | ForestFromLabelsError (NEForestFromLabelsError a)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Sort from lowest to highest at each level in the 'Tree'.
--
-- @since 0.1.0.0
sortTree :: (Ord a) => Tree a -> Tree a
sortTree = sortTreeOn id

-- | Sort from lowest to highest at each level in the 'Tree', using the results
-- of a key function applied to each label.
--
-- @since 0.1.0.0
sortTreeOn :: (Ord b) => (a -> b) -> Tree a -> Tree a
sortTreeOn f =
  Tree.unfoldTree \Node { rootLabel, subForest } ->
    (rootLabel, List.sortOn (f . Tree.rootLabel) subForest)

-- | For each 'Tree' in the 'Forest', sort from lowest to highest at each level
-- in the 'Tree'. The 'Forest' itself is also sorted from lowest to highest via
-- the root labels of each 'Tree' in the 'Forest'.
--
-- @since 0.1.0.0
sortForest :: (Ord a) => Forest a -> Forest a
sortForest = sortForestOn id

-- | For each 'Tree' in the 'Forest', sort from lowest to highest at each level
-- in the 'Tree', using the results of a key function applied at each label. The
-- 'Forest' itself is also sorted from lowest to highest via applying the key
-- function to the root labels of each 'Tree' in the 'Forest'.
--
-- @since 0.1.0.0
sortForestOn :: (Ord b) => (a -> b) -> Forest a -> Forest a
sortForestOn f =
  List.sortOn (f . Tree.rootLabel) . map (sortTreeOn f)

-- | For each 'Tree' in the 'NEForest', sort from lowest to highest at each
-- level in the 'Tree'. The 'NEForest' itself is also sorted from lowest to
-- highest via the root labels of each 'Tree' in the 'NEForest'.
--
-- @since 0.1.0.0
sortNEForest :: (Ord a) => NEForest a -> NEForest a
sortNEForest = sortNEForestOn id

-- | For each 'Tree' in the 'NEForest', sort from lowest to highest at each
-- level in the 'Tree', using the results of a key function applied at each
-- label. The 'NEForest' itself is also sorted from lowest to highest via
-- applying the key function to the root labels of each 'Tree' in the
-- 'NEForest'.
--
-- @since 0.1.0.0
sortNEForestOn :: (Ord b) => (a -> b) -> NEForest a -> NEForest a
sortNEForestOn f forest =
  NonEmpty.sortBy (Ord.comparing (f . Tree.rootLabel))
    $ NonEmpty.map (sortTreeOn f) forest

-- | Find a particular 'Node' in a 'Tree' via the provided label predicate.
-- Unlike 'Foldable.find', this function will return the entire subtree instead
-- of just the label value.
--
-- @since 0.1.0.0
findNodeInTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
findNodeInTree p = \case
  node@Node { rootLabel, subForest }
    | p rootLabel -> Just node
    | otherwise -> findNodeInForest p subForest

-- | Check if the first 'Tree' is a subtree of the second, meaning each level
-- of labels in the first 'Tree' exists in a subtree of the second regardless
-- of the label ordering at each level.
--
-- @since 0.1.0.0
isSubtreeOf :: (Ord a) => Tree a -> Tree a -> Bool
isSubtreeOf = isSubtreeOfBy ((==) `Function.on` sortTree)

-- | Check if the first 'Tree' is an exact subtree of the second, meaning each
-- level of labels in the first 'Tree' exists in the same order in a subtree
-- of the second.
--
-- @since 0.1.0.0
isExactSubtreeOf :: (Eq a) => Tree a -> Tree a -> Bool
isExactSubtreeOf = isSubtreeOfBy (==)

-- | Check if the first 'Tree' is a subtree of the second, where the subtree
-- determination is done via applying the binary predicate to the first 'Tree'
-- and each node in the second 'Tree'.
--
-- This is a lower-level function. Users should prefer 'isSubtreeOf' and
-- 'isExactSubtreeOf' over this function.
--
-- @since 0.1.0.0
isSubtreeOfBy
  :: (Eq a)
  => (Tree a -> Tree a -> Bool)
  -> Tree a
  -> Tree a
  -> Bool
isSubtreeOfBy f subtree = \case
  tree@Node { subForest }
    | f subtree tree -> True
    | otherwise -> or $ map (isSubtreeOfBy f subtree) subForest

-- | Find a particular 'Node' in a forest via the provided label predicate. This
-- function delegates to 'findNodeInTree' for each 'Tree' in the forest.
--
-- @since 0.1.0.0
findNodeInForest :: (Foldable t) => (a -> Bool) -> t (Tree a) -> Maybe (Tree a)
findNodeInForest p forest =
  Monoid.getAlt $ Foldable.foldMap (Alt . findNodeInTree p) forest

-- | Check if the 'Tree' is a subtree in the forest, meaning each level of
-- labels in the 'Tree' exists in a subtree of some 'Tree' in the forest
-- regardless of the label ordering at each level.
--
-- @since 0.1.0.0
isSubtreeIn :: (Foldable t, Ord a) => Tree a -> t (Tree a) -> Bool
isSubtreeIn = isSubtreeInBy ((==) `Function.on` sortTree)

-- | Check if the 'Tree' is an exact subtree in the forest, meaning each
-- level of labels in the 'Tree' exists in the same order in a subtree
-- of some 'Tree' in the forest.
--
-- @since 0.1.0.0
isExactSubtreeIn :: (Eq a, Foldable t) => Tree a -> t (Tree a) -> Bool
isExactSubtreeIn = isSubtreeInBy (==)

-- | Check if the 'Tree' is a subtree in the forest, where the subtree
-- determination is done via applying the binary predicate to the first 'Tree'
-- and each node in each 'Tree' in the forest.
--
-- This is a lower-level function. Users should prefer 'isSubtreeIn' and
-- 'isExactSubtreeIn' over this function.
--
-- @since 0.1.0.0
isSubtreeInBy
  :: (Eq a, Foldable t)
  => (Tree a -> Tree a -> Bool)
  -> Tree a
  -> t (Tree a)
  -> Bool
isSubtreeInBy f subtree = or . map (isSubtreeOfBy f subtree) . Foldable.toList

-- | Number each level of labels in the tree, starting from 0 at each level.
--
-- @since 0.1.0.0
enumerateTree :: (Enum a, Num a) => Tree b -> Tree (a, b)
enumerateTree = Zip.mzip indicesTree

-- | Number each level of labels in the 'Forest', starting from 0 at each level.
--
-- @since 0.1.0.0
enumerateForest :: (Enum a, Num a) => Forest b -> Forest (a, b)
enumerateForest = zipForest indicesForest

-- | Number each level of labels in the 'NEForest', starting from 0 at each
-- level.
--
-- @since 0.1.0.0
enumerateNEForest :: (Enum a, Num a) => NEForest b -> NEForest (a, b)
enumerateNEForest = zipNEForest indicesNEForest

-- | Apply a function to each label in each 'Tree' in the 'Forest'.
--
-- @since 0.1.0.0
mapForest :: (a -> b) -> Forest a -> Forest b
mapForest f = map (fmap f)

-- | Apply a function to each label in each 'Tree' in the 'NEForest'.
--
-- @since 0.1.0.0
mapNEForest :: (a -> b) -> NEForest a -> NEForest b
mapNEForest f = NonEmpty.map (fmap f)

-- | Given two input 'Forest' values, provide a 'Forest' of corresponding label
-- pairs.
--
-- @since 0.1.0.0
zipForest :: Forest a -> Forest b -> Forest (a, b)
zipForest = zipWithForest (,)

-- | Given two input 'NEForest' values, provide an 'NEForest' of corresponding
-- label pairs.
--
-- @since 0.1.0.0
zipNEForest :: NEForest a -> NEForest b -> NEForest (a, b)
zipNEForest = zipWithNEForest (,)

-- | Generalizes 'zipForest' by zipping label values via the provided function.
--
-- @since 0.1.0.0
zipWithForest :: (a -> b -> c) -> Forest a -> Forest b -> Forest c
zipWithForest f = zipWith (Zip.mzipWith f)

-- | Generalizes 'zipNEForest' by zipping label values via the provided
-- function.
--
-- @since 0.1.0.0
zipWithNEForest :: (a -> b -> c) -> NEForest a -> NEForest b -> NEForest c
zipWithNEForest f = NonEmpty.zipWith (Zip.mzipWith f)

-- | Flatten each 'Tree' in the input 'Forest', then concatenate the results.
--
-- @since 0.1.0.0
flattenForest :: Forest a -> [a]
flattenForest = concatMap Tree.flatten

-- | Flatten each 'Tree' in the input 'NEForest', then concatenate the results.
--
-- @since 0.1.0.0
flattenNEForest :: NEForest a -> NonEmpty a
flattenNEForest forest =
  Semigroup.sconcat
    $ NonEmpty.map (NonEmpty.fromList . Tree.flatten) forest

-- | Creates a 'Tree' containing the provided label and no children.
--
-- @since 0.1.0.0
singletonTree :: a -> Tree a
singletonTree = pure

-- | Produces all subtrees of the given 'Tree'.
--
-- The output is a 'Forest' out of convenience, but is guaranteed non-empty as
-- a 'Tree' itself is non-empty by construction. See 'neSubtrees' for a variant
-- that returns an 'NEForest'.
--
-- @since 0.1.0.0
subtrees :: Tree a -> Forest a
subtrees tree@Node { subForest } = tree : (subForest >>= subtrees)

-- | Produces all subtrees of the given 'Tree'.
--
-- @since 0.1.0.0
neSubtrees :: Tree a -> NEForest a
neSubtrees = NonEmpty.fromList . subtrees

-- | Produce an infinite 'Tree' of indices, starting from 0 at each level.
--
-- @since 0.1.0.0
indicesTree :: (Enum a, Num a) => Tree a
indicesTree = Tree.unfoldTree (flip (,) [0..]) 0

-- | Build a 'Tree' from a flat input list of labels.
--
-- @since 0.1.0.0
eitherTreeFromLabels
  :: (a -> Bool) -- ^ Is the label a root?
  -> (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> NonEmpty a -- ^ Flat list of labels
  -> Either (TreeFromLabelsError a) (Tree a)
eitherTreeFromLabels isRoot isImmediateChildOf labels = do
  case eitherNEForestFromLabels isRoot isImmediateChildOf labels of
    Left err -> Left $ ForestFromLabelsError err
    Right forest ->
      case forest of
        tree :| [] -> Right tree
        _tree :| _trees -> Left $ TooManyTreesError forest

-- | Build a 'Tree' from a flat input list of labels.
--
-- Throws 'FromLabelsError' if anything goes wrong when building the 'Tree'.
--
-- @since 0.1.0.0
unsafeTreeFromLabels
  :: (Show a, Typeable a)
  => (a -> Bool) -- ^ Is the label a root?
  -> (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> NonEmpty a -- ^ Flat list of labels
  -> Tree a
unsafeTreeFromLabels isRoot isImmediateChildOf labels = do
  case eitherTreeFromLabels isRoot isImmediateChildOf labels of
    Left ex -> Ex.throw ex
    Right tree -> tree

-- | Creates a 'Forest' containing a single 'Tree' that contains the provided
-- label and no children.
--
-- @since 0.1.0.0
singletonForest :: a -> Forest a
singletonForest = pure . singletonTree

-- | Creates an 'NEForest' containing a single 'Tree' that contains the provided
-- label and no children.
--
-- @since 0.1.0.0
singletonNEForest :: a -> NEForest a
singletonNEForest = pure . singletonTree

-- | Produce an infinite 'Forest' of indices, starting from 0 at each level.
--
-- @since 0.1.0.0
indicesForest :: (Enum a, Num a) => Forest a
indicesForest = Tree.unfoldForest (flip (,) [0..]) [0..]

-- | Produce an infinite 'NEForest' of indices, starting from 0 at each level.
--
-- @since 0.1.0.0
indicesNEForest :: (Enum a, Num a) => NEForest a
indicesNEForest = NonEmpty.fromList indicesForest

-- | Build an 'NEForest' from a flat input list of labels.
--
-- @since 0.1.0.0
eitherNEForestFromLabels
  :: forall a. (a -> Bool) -- ^ Is the label a root?
  -> (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> NonEmpty a -- ^ Flat list of labels
  -> Either (NEForestFromLabelsError a) (NEForest a)
eitherNEForestFromLabels isRoot isImmediateChildOf labels =
  case NonEmpty.partition isRoot labels of
    ([], _children) -> Left $ NoRootsFoundError labels
    (rootLabels, childLabels) -> result where
      result =
        case forestWithOrphans of
          (forest, (orphan : orphans)) ->
            Left $ OrphansFoundError forest $ orphan :| orphans
          (forest, []) ->
            -- This NonEmpty conversion is righteous due to the input NonEmpty.
            Right $ unsafeNEForest forest

      forestWithOrphans :: (Forest a, [a])
      forestWithOrphans = do
        flip State.runState childLabels do
          traverse (Tree.unfoldTreeM parentChildrenPair) rootLabels

      parentChildrenPair :: a -> State [a] (a, [a])
      parentChildrenPair parent = do
        (children, rest) <- fmap partitionChildren State.get
        State.put rest
        pure (parent, children)
        where
        partitionChildren :: [a] -> ([a], [a])
        partitionChildren =
          List.partition \possibleChild ->
            possibleChild `isImmediateChildOf` parent

-- | Build an 'NEForest' from a flat input list of labels.
--
-- Throws 'NEForestFromLabelsError' if anything goes wrong when building the 'NEForest'.
--
-- @since 0.1.0.0
unsafeNEForestFromLabels
  :: (Show a, Typeable a)
  => (a -> Bool) -- ^ Is the label a root?
  -> (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> NonEmpty a -- ^ Flat list of labels
  -> NEForest a
unsafeNEForestFromLabels isRoot isImmediateChildOf labels = do
  case eitherNEForestFromLabels isRoot isImmediateChildOf labels of
    Left ex -> Ex.throw ex
    Right forest -> forest

-- | Build an 'NEForest' from a 'Forest', producing 'Nothing' if the input
-- 'Forest' is empty.
--
-- @since 0.1.0.0
neForest :: Forest a -> Maybe (NEForest a)
neForest = \case
  [] -> Nothing
  t : ts -> Just $ t :| ts

-- | Build an 'NEForest' from a 'Forest', raising an error if the input 'Forest'
-- is empty.
--
-- @since 0.1.0.0
unsafeNEForest :: Forest a -> NEForest a
unsafeNEForest forest =
  case neForest forest of
    Nothing -> error "Rosebud.unsafeNEForest: empty forest"
    Just forest' -> forest'

-- $intro
--
-- This module captures functions and patterns often reached for when working
-- with "Data.Tree" from the @containers@ package.
