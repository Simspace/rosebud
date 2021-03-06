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
  , isSubtreeOfUsing
    -- ** Forests
  , findNodeInForest
  , isSubtreeIn
  , isExactSubtreeIn
  , isSubtreeInUsing

    -- * Transformation
    -- ** Trees
  , enumerateTree
  , zipTree
  , zipWithTree
  , pathsTree
  , leavesTree
    -- ** Forests
  , enumerateForest
  , enumerateNEForest
  , mapForest
  , mapNEForest
  , zipForest
  , zipNEForest
  , zipWithForest
  , zipWithNEForest
  , pathsForest
  , pathsNEForest
  , leavesForest
  , leavesNEForest
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
  , eitherNEForestFromPartitionedLabels
  , unsafeNEForestFromPartitionedLabels
  , eitherNEForestFromLabels
  , unsafeNEForestFromLabels
  , neForest
  , unsafeNEForest

    -- * Errors
  , FromPartitionedLabelsError(OrphansFoundError)
  , FromLabelsError(NoRootsFoundError, FromPartitionedLabels)
  ) where

import Control.Exception (Exception)
import Control.Monad.Trans.State (State)
import Data.List.NonEmpty (NonEmpty((:|)), NonEmpty)
import Data.Monoid (Alt(Alt))
import Data.Sequence ((<|), Seq)
import Data.Tree (Tree(Node, rootLabel, subForest), Forest)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prelude
import qualified Control.Exception as Ex
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Zip as Zip
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord
import qualified Data.Semigroup as Semigroup
import qualified Data.Traversable as Traversable
import qualified Data.Tree as Tree

-- | A convenience type alias for a non-empty 'Forest'.
--
-- @since 0.1.0.0
type NEForest a = NonEmpty (Tree a)

-- | The error type when building a 'Tree'/'NEForest' from labels already
-- partitioned into roots and children.
--
-- @since 0.2.0.0
data FromPartitionedLabelsError a
  = -- | Orphan labels were found. Provides the assembled 'NEForest' and a flat
    -- list of orphan labels.
    --
    -- @since 0.2.0.0
    OrphansFoundError (NEForest a) (NonEmpty a)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | The error type when building an 'NEForest' from a flat list of labels.
--
-- @since 0.2.0.0
data FromLabelsError a
  = -- | No root label(s) were found. Provides the flat list of input labels.
    --
    -- @since 0.2.0.0
    NoRootsFoundError (NonEmpty a)
    -- | Produced via internally building from partitioned labels.
    --
    -- @since 0.2.0.0
  | FromPartitionedLabels (FromPartitionedLabelsError a)
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
isSubtreeOf = isSubtreeOfUsing sortTree

-- | Check if the first 'Tree' is an exact subtree of the second, meaning each
-- level of labels in the first 'Tree' exists in the same order in a subtree
-- of the second.
--
-- @since 0.1.0.0
isExactSubtreeOf :: (Eq a) => Tree a -> Tree a -> Bool
isExactSubtreeOf = isSubtreeOfUsing id

-- | Check if the first 'Tree' is a subtree of the second via equality of the
-- first 'Tree' with any node in the second 'Tree'.
--
-- This is a lower-level function. Users should prefer 'isSubtreeOf' and
-- 'isExactSubtreeOf' over this function. The function argument enables
-- pre-processing over the 'Tree' values involved, before equality-checking is
-- performed.
--
-- @since 0.1.0.0
isSubtreeOfUsing
  :: forall a. (Eq a)
  => (Tree a -> Tree a) -- ^ Transforms 'Tree' values prior to equality-checking
  -> Tree a
  -> Tree a
  -> Bool
isSubtreeOfUsing f subtree = go . f
  where
  go :: Tree a -> Bool
  go = \case
    tree@Node { subForest }
      | subtree' == tree -> True
      | otherwise -> or $ map go subForest

  subtree' :: Tree a
  subtree' = f subtree

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
isSubtreeIn = isSubtreeInUsing sortTree

-- | Check if the 'Tree' is an exact subtree in the forest, meaning each
-- level of labels in the 'Tree' exists in the same order in a subtree
-- of some 'Tree' in the forest.
--
-- @since 0.1.0.0
isExactSubtreeIn :: (Eq a, Foldable t) => Tree a -> t (Tree a) -> Bool
isExactSubtreeIn = isSubtreeInUsing id

-- | Check if the first 'Tree' is a subtree in the forest via equality of the
-- first 'Tree' with any node in any 'Tree' in the forest.
--
-- This is a lower-level function. Users should prefer 'isSubtreeIn' and
-- 'isExactSubtreeIn' over this function. The function argument enables
-- pre-processing over the 'Tree' values involved, before equality-checking is
-- performed.
--
-- @since 0.1.0.0
isSubtreeInUsing
  :: (Eq a, Foldable t)
  => (Tree a -> Tree a) -- ^ Transforms 'Tree' values prior to equality-checking
  -> Tree a
  -> t (Tree a)
  -> Bool
isSubtreeInUsing f subtree =
  or . map (isSubtreeOfUsing f subtree) . Foldable.toList

-- | Number each level of labels in the tree, starting from 0 at each level.
--
-- @since 0.1.0.0
enumerateTree :: (Enum a, Num a) => Tree b -> Tree (a, b)
enumerateTree = Zip.mzip indicesTree

-- | Given two input 'Tree' values, provide a 'Tree' of corresponding label
-- pairs. This function exists for the convenience of not needing to import
-- "Control.Monad.Zip".
--
-- @since 0.1.0.0
zipTree :: Tree a -> Tree b -> Tree (a, b)
zipTree = Zip.mzip

-- | Generalizes 'zipTree' by zipping label values via the provided function.
-- This function exists for the convenience of not needing to import
-- "Control.Monad.Zip".
--
-- @since 0.1.0.0
zipWithTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithTree f = Zip.mzipWith f

-- | Produce all the paths for the given 'Tree'.
--
-- > λ> pathsTree $ Node 1 [Node 2 [Node 4 [], Node 5 []], Node 3 []]
-- > fromList [1] :| [fromList [1,2],fromList [1,2,4],fromList [1,2,5],fromList [1,3]]
--
-- @since 0.1.0.0
pathsTree :: forall a. Tree a -> NonEmpty (Seq a)
pathsTree = NonEmpty.fromList . go
  where
  go :: Tree a -> [Seq a]
  go = \case
    Node { rootLabel, subForest } ->
      pure rootLabel : concatMap (map (rootLabel <|) . go) subForest

-- | Produce all the leaves for the given 'Tree'.
--
-- @since 0.1.0.0
leavesTree :: Tree a -> NonEmpty a
leavesTree = NonEmpty.fromList . go
  where
  go = \case
    Node { rootLabel, subForest }
      | null subForest -> rootLabel : rest
      | otherwise -> rest
      where
      rest = concatMap go subForest

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

-- | Produce all the paths for the given 'Forest', if any 'Tree' values exist
-- in the 'Forest'.
--
-- @since 0.1.0.0
pathsForest :: Forest a -> Maybe (NonEmpty (Seq a))
pathsForest = \case
  [] -> Nothing
  forest -> Just $ pathsNEForest $ NonEmpty.fromList forest

-- | Produce all the paths for the given 'NEForest'.
--
-- @since 0.1.0.0
pathsNEForest :: NEForest a -> NonEmpty (Seq a)
pathsNEForest = Semigroup.sconcat . NonEmpty.map pathsTree

-- | Produce all the leaves for the given 'Forest', if any 'Tree' values exist
-- in the 'Forest.
--
-- @since 0.1.0.0
leavesForest :: Forest a -> Maybe (NonEmpty a)
leavesForest = \case
  [] -> Nothing
  forest -> Just $ leavesNEForest $ NonEmpty.fromList forest

-- | Produce all the leaves for the given 'NEForest'.
--
-- @since 0.1.0.0
leavesNEForest :: NEForest a -> NonEmpty a
leavesNEForest = Semigroup.sconcat . NonEmpty.map leavesTree

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

-- | Build a 'Tree' from a root label and a flat list of child labels.
--
-- @since 0.2.0.0
eitherTreeFromLabels
  :: (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> a -- ^ Root label
  -> [a] -- ^ Flat list of child labels
  -> Either (FromPartitionedLabelsError a) (Tree a)
eitherTreeFromLabels isImmediateChildOf root children = do
  Bifunctor.second NonEmpty.head
    $ eitherNEForestFromPartitionedLabels isImmediateChildOf (pure root) children

-- | Build a 'Tree' from a root label and a flat list of child labels.
--
-- Throws 'FromPartitionedLabelsError' if anything goes wrong when building
-- the 'Tree'.
--
-- @since 0.2.0.0
unsafeTreeFromLabels
  :: (Show a, Typeable a)
  => (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> a -- ^ Root label
  -> [a] -- ^ Flat list of labels
  -> Tree a
unsafeTreeFromLabels isImmediateChildOf root children = do
  either Ex.throw id $ eitherTreeFromLabels isImmediateChildOf root children

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

-- | Build an 'NEForest' from flat input lists of root and child labels.
--
-- @since 0.2.0.0
eitherNEForestFromPartitionedLabels
  :: forall a. (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> NonEmpty a -- ^ Flat list of root labels
  -> [a] -- ^ Flat list of child labels
  -> Either (FromPartitionedLabelsError a) (NEForest a)
eitherNEForestFromPartitionedLabels isImmediateChildOf roots children =
  case forestWithOrphans of
    (forest, (orphan : orphans)) ->
      Left $ OrphansFoundError forest $ orphan :| orphans
    (forest, []) ->
      Right forest
  where
  forestWithOrphans :: (NEForest a, [a])
  forestWithOrphans = do
    flip State.runState children do
      Traversable.for roots \rootLabel -> do
        Tree.unfoldTreeM parentChildrenPair rootLabel

  parentChildrenPair :: a -> State [a] (a, [a])
  parentChildrenPair parent = do
    (cs, rest) <- fmap partitionChildren State.get
    State.put rest
    pure (parent, cs)
    where
    partitionChildren :: [a] -> ([a], [a])
    partitionChildren =
      List.partition \possibleChild ->
        possibleChild `isImmediateChildOf` parent

-- | Build an 'NEForest' from flat input lists of root and child labels.
--
-- Throws 'FromPartitionedLabelsError' if anything goes wrong when building
-- the 'NEForest'.
--
-- @since 0.2.0.0
unsafeNEForestFromPartitionedLabels
  :: (Show a, Typeable a)
  => (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> NonEmpty a -- ^ Flat list of root labels
  -> [a] -- ^ Flat list of child labels
  -> NEForest a
unsafeNEForestFromPartitionedLabels isImmediateChildOf roots children =
  either Ex.throw id
    $ eitherNEForestFromPartitionedLabels isImmediateChildOf roots children

-- | Build an 'NEForest' from a flat input list of labels.
--
-- @since 0.1.0.0
eitherNEForestFromLabels
  :: forall a. (a -> Bool) -- ^ Is the label a root?
  -> (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> NonEmpty a -- ^ Flat list of labels
  -> Either (FromLabelsError a) (NEForest a)
eitherNEForestFromLabels isRoot isImmediateChildOf labels =
  case NonEmpty.partition isRoot labels of
    ([], _children) -> Left $ NoRootsFoundError labels
    (roots, children) ->
      Bifunctor.first FromPartitionedLabels
        $ eitherNEForestFromPartitionedLabels
            isImmediateChildOf
            (NonEmpty.fromList roots)
            children

-- | Build an 'NEForest' from a flat input list of labels.
--
-- Throws 'FromLabelsError' if anything goes wrong when building the 'NEForest'.
--
-- @since 0.1.0.0
unsafeNEForestFromLabels
  :: (Show a, Typeable a)
  => (a -> Bool) -- ^ Is the label a root?
  -> (a -> a -> Bool) -- ^ Is the first label an immediate child of the second?
  -> NonEmpty a -- ^ Flat list of labels
  -> NEForest a
unsafeNEForestFromLabels isRoot isImmediateChildOf labels =
  either Ex.throw id $ eitherNEForestFromLabels isRoot isImmediateChildOf labels

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
