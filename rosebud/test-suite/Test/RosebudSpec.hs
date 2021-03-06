{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.RosebudSpec
  ( spec
  ) where

import Control.Exception (ErrorCall(ErrorCall))
import Data.List.NonEmpty (NonEmpty((:|)))
import Rosebud (Tree(Node, subForest), Forest, NEForest)
import Test.Hspec
import Test.QuickCheck ((==>))
import Test.Rosebud.Arbitrary ()
import qualified Control.Exception as Exception
import qualified Control.Monad.Zip as Zip
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Sequence
import qualified Data.Tree as Tree
import qualified Rosebud
import qualified Test.QuickCheck as QC

-- N.B. The following functions are not explicitly under test in this module,
-- but are tested indirectly via other functions:
--
-- +-----------------+-------------------+
-- |    Function     |    Tested Via     |
-- +-----------------+-------------------+
-- | indicesTree     | enumerateTree     |
-- | indicesForest   | enumerateForest   |
-- | indicesNEForest | enumerateNEForest |
-- +-----------------+-------------------+

spec :: Spec
spec = parallel do
  describe "sortTree" do
    it "tree size stays the same" do
      QC.property \(x :: Tree Int) ->
        Foldable.length (Rosebud.sortTree x) `shouldBe` Foldable.length x
    it "children are sorted" do
      QC.property \(x :: Tree Int) ->
        childrenSortedBy (<=) $ Rosebud.sortTree x

  describe "sortTreeOn" do
    it "tree size stays the same" do
      QC.property \(x :: Tree Int) ->
        Foldable.length (Rosebud.sortTreeOn negate x) `shouldBe` Foldable.length x
    it "children are sorted" do
      QC.property \(x :: Tree Int) ->
        childrenSortedBy (>=) $ Rosebud.sortTreeOn negate x

  describe "sortForest" do
    it "forest size stays the same" do
      QC.property \(x :: Forest Int) ->
        sum (fmap (Foldable.length) (Rosebud.sortForest x))
          `shouldBe` sum (fmap Foldable.length x)
    it "forest is sorted by the root nodes" do
      QC.property \(x :: Forest Int) ->
         isSortedBy (<=) $ fmap Tree.rootLabel $ Rosebud.sortForest x
    it "each tree's children are sorted" do
      QC.property \(x :: Forest Int) ->
        and $ fmap (childrenSortedBy (<=)) $ Rosebud.sortForest x

  describe "sortForestOn" do
    it "forest size stays the same" do
      QC.property \(x :: Forest Int) ->
        sum (fmap (Foldable.length) (Rosebud.sortForestOn negate x))
          `shouldBe` sum (fmap Foldable.length x)
    it "forest is sorted by the root nodes" do
      QC.property \(x :: Forest Int) ->
         isSortedBy (>=) $ fmap Tree.rootLabel $ Rosebud.sortForestOn negate x
    it "each tree's children are sorted" do
      QC.property \(x :: Forest Int) ->
        and $ fmap (childrenSortedBy (>=)) $ Rosebud.sortForestOn negate x

  describe "sortNEForest" do
    it "forest size stays the same" do
      QC.property \(x :: NEForest Int) ->
        sum (fmap (Foldable.length) (Rosebud.sortNEForest x))
          `shouldBe` sum (fmap Foldable.length x)
    it "forest is sorted by the root nodes" do
      QC.property \(x :: NEForest Int) ->
         isSortedBy (<=) $ NonEmpty.toList $ fmap Tree.rootLabel $ Rosebud.sortNEForest x
    it "each tree's children are sorted" do
      QC.property \(x :: NEForest Int) ->
        and $ fmap (childrenSortedBy (<=)) $ Rosebud.sortNEForest x

  describe "sortNEForestOn" do
    it "forest size stays the same" do
      QC.property \(x :: NEForest Int) ->
        sum (fmap (Foldable.length) (Rosebud.sortNEForestOn negate x))
          `shouldBe` sum (fmap Foldable.length x)
    it "forest is sorted by the root nodes" do
      QC.property \(x :: NEForest Int) ->
         isSortedBy (>=) $ NonEmpty.toList $ fmap Tree.rootLabel $ Rosebud.sortNEForestOn negate x
    it "each tree's children are sorted" do
      QC.property \(x :: NEForest Int) ->
        and $ fmap (childrenSortedBy (>=)) $ Rosebud.sortNEForestOn negate x

  describe "findNodeInTree" do
    it "can fail to find a node" do
      Rosebud.findNodeInTree (== 'e') (Node 'a' [Node 'b' [Node 'c' []], Node 'd' []])
        `shouldBe` Nothing
    it "can find a node" do
      Rosebud.findNodeInTree (== 'b') (Node 'a' [Node 'b' [Node 'c' []], Node 'd' []])
        `shouldBe` Just (Node 'b' [Node 'c' []])

  describe "isSubtreeOf" do
    it "fails when subtree not present" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'c' [], Node 'e' []]
      Rosebud.isSubtreeOf subtree tree `shouldBe` False
    it "passes regardless of child ordering" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'd' [], Node 'c' []]
      Rosebud.isSubtreeOf subtree tree `shouldBe` True
    it "passes when children are in same order" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'c' [], Node 'd' []]
      Rosebud.isSubtreeOf subtree tree `shouldBe` True

  describe "isExactSubtreeOf" do
    it "fails when subtree not present" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'c' [], Node 'e' []]
      Rosebud.isExactSubtreeOf subtree tree `shouldBe` False
    it "fails when children are not in same order" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'd' [], Node 'c' []]
      Rosebud.isExactSubtreeOf subtree tree `shouldBe` False
    it "passes when children are in same order" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'c' [], Node 'd' []]
      Rosebud.isExactSubtreeOf subtree tree `shouldBe` True

  describe "isSubtreeOfUsing" do
    it "fails when subtree not present" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'c' [], Node 'e' []]
      Rosebud.isSubtreeOfUsing id subtree tree `shouldBe` False
    it "fails when children are not in same order" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'd' [], Node 'c' []]
      Rosebud.isSubtreeOfUsing id subtree tree `shouldBe` False
    it "passes when children are in same order" do
      let tree    = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]
      let subtree =           Node 'b' [Node 'c' [], Node 'd' []]
      Rosebud.isSubtreeOfUsing id subtree tree `shouldBe` True

  describe "findNodeInForest" do
    it "can fail to find a node" do
      Rosebud.findNodeInForest (== 'e')
        [ Node 'a' []
        , Node 'b'
            [ Node 'c' []
            ]
        , Node 'd' []
        ] `shouldBe` Nothing
    it "can find a node" do
      Rosebud.findNodeInForest (== 'b')
        [ Node 'a' []
        , Node 'b'
            [ Node 'c' []
            ]
        , Node 'd' []
        ] `shouldBe` Just (Node 'b' [Node 'c' []])

  describe "isSubtreeIn" do
    it "fails when subtree not present" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'c' [], Node 'e' []]
      Rosebud.isSubtreeIn subtree tree `shouldBe` False
    it "passes regardless of child ordering" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'd' [], Node 'c' []]
      Rosebud.isSubtreeIn subtree tree `shouldBe` True
    it "passes when children are in same order" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'c' [], Node 'd' []]
      Rosebud.isSubtreeIn subtree tree `shouldBe` True

  describe "isExactSubtreeIn" do
    it "fails when subtree not present" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'c' [], Node 'e' []]
      Rosebud.isExactSubtreeIn subtree tree `shouldBe` False
    it "fails when children are not in same order" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'd' [], Node 'c' []]
      Rosebud.isExactSubtreeIn subtree tree `shouldBe` False
    it "passes when children are in same order" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'c' [], Node 'd' []]
      Rosebud.isExactSubtreeIn subtree tree `shouldBe` True

  describe "isSubtreeInUsing" do
    it "fails when subtree not present (no tree pre-processing)" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'c' [], Node 'e' []]
      Rosebud.isSubtreeInUsing id subtree tree `shouldBe` False
    it "fails when children are not in same order (no tree pre-processing)" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'd' [], Node 'c' []]
      Rosebud.isSubtreeInUsing id subtree tree `shouldBe` False
    it "passes when children are in same order (no tree pre-processing)" do
      let tree    = [Node 'z' [], Node 'a' [Node 'b' [Node 'c' [], Node 'd' []]]]
      let subtree =                         Node 'b' [Node 'c' [], Node 'd' []]
      Rosebud.isSubtreeInUsing id subtree tree `shouldBe` True

  describe "enumerateTree" do
    it "singleton" do
      let tree = Node 'a' []
      Rosebud.enumerateTree @Int tree `shouldBe` Node (0, 'a') []
    it "some children" do
      let tree = Node 'a' [Node 'b' [], Node 'c' [Node 'd' []]]
      Rosebud.enumerateTree @Int tree
        `shouldBe`
          Node (0, 'a') [Node (0, 'b') [], Node (1, 'c') [Node (0, 'd') []]]

  describe "zipTree" do
    it "equivalent to 'Control.Monad.Zip.mzip'" do
      QC.property \(x :: Tree Int) (y :: Tree Int) ->
        Rosebud.zipTree x y `shouldBe` Zip.mzip x y

  describe "zipWithTree" do
    it "equivalent to 'Control.Monad.Zip.mzipWith'" do
      QC.property \(x :: Tree Int) (y :: Tree Int) ->
        Rosebud.zipWithTree (+) x y `shouldBe` Zip.mzipWith (+) x y

  describe "pathsTree" do
    it "singleton" do
      let tree = Node 'a' []
      Rosebud.pathsTree tree
        `shouldBe` NonEmpty.fromList [Sequence.fromList ['a']]
    it "some children" do
      let tree =
            Node 'a'
              [ Node 'b'
                  [ Node 'c' []
                  , Node 'd' []
                  ]
              , Node 'e' []
              ]
      Rosebud.pathsTree tree
        `shouldBe`
          NonEmpty.fromList
            [ Sequence.fromList "a"
            , Sequence.fromList "ab"
            , Sequence.fromList "abc"
            , Sequence.fromList "abd"
            , Sequence.fromList "ae"
            ]

  describe "leavesTree" do
    it "singleton" do
      let tree = Node 'a' []
      Rosebud.leavesTree tree
        `shouldBe` NonEmpty.fromList ['a']
    it "some children" do
      let tree =
            Node 'a'
              [ Node 'b'
                  [ Node 'c' []
                  , Node 'd' []
                  ]
              , Node 'e' []
              ]
      Rosebud.leavesTree tree `shouldBe` NonEmpty.fromList ['c', 'd', 'e']

  describe "enumerateForest" do
    it "singleton" do
      let forest = [Node 'a' []]
      Rosebud.enumerateForest @Int forest `shouldBe` [Node (0, 'a') []]
    it "some children" do
      let forest = [Node 'a' [Node 'b' [], Node 'c' [Node 'd' []]]]
      Rosebud.enumerateForest @Int forest
        `shouldBe`
          [Node (0, 'a') [Node (0, 'b') [], Node (1, 'c') [Node (0, 'd') []]]]

  describe "enumerateNEForest" do
    it "equivalent to 'enumerateForest' for non-empty" do
      QC.property \(x :: NEForest Int) ->
        NonEmpty.toList (Rosebud.enumerateNEForest @Int x)
          `shouldBe` Rosebud.enumerateForest (NonEmpty.toList x)

  describe "mapForest" do
    it "equivalent to 'map (fmap f)" do
      QC.property \(x :: Forest Int) ->
        Rosebud.mapForest show x `shouldBe` map (fmap show) x
    it "identity" do
      QC.property \(x :: Forest Int) ->
        Rosebud.mapForest id x `shouldBe` x
    it "composition" do
      QC.property \(x :: Forest Int) ->
        let f = (+ 2)
            g = show
         in Rosebud.mapForest (g . f) x
              `shouldBe` (Rosebud.mapForest g . Rosebud.mapForest f) x

  describe "mapNEForest" do
    it "equivalent to 'mapForest' for non-empty" do
      QC.property \(x :: NEForest Int) ->
        NonEmpty.toList (Rosebud.mapNEForest show x)
          `shouldBe` Rosebud.mapForest show (NonEmpty.toList x)

  describe "zipForest" do
    it "equivalent to 'zipWithForest (,)'" do
      QC.property \(x :: Forest Int) (y :: Forest Int) ->
        Rosebud.zipForest x y `shouldBe` Rosebud.zipWithForest (,) x y

  describe "zipNEForest" do
    it "equivalent to 'zipWithNEForest (,)'" do
      QC.property \(x :: NEForest Int) (y :: NEForest Int) ->
        Rosebud.zipNEForest x y `shouldBe` Rosebud.zipWithNEForest (,) x y

  describe "zipWithForest" do
    it "singletons" do
      let f x y = List.intercalate "|" [x, y]
      let forest1 = [Node "1" []]
      let forest2 = [Node "2" []]
      Rosebud.zipWithForest f forest1 forest2 `shouldBe` [Node "1|2" []]
    it "some children" do
      let f x y = List.intercalate "|" [x, y]
      let forest1 = [Node "1" [Node "11" [], Node "12" []]]
      let forest2 = [Node "2" [Node "21" [], Node "22" [Node "211" []]]]
      Rosebud.zipWithForest f forest1 forest2
        `shouldBe`
          [Node "1|2" [Node "11|21" [], Node "12|22" []]]

  describe "zipWithNEForest" do
    it "equivalent to 'zipWithForest' for non-empty" do
      let asList = NonEmpty.toList
      QC.property \(x :: NEForest Int) (y :: NEForest Int) ->
        asList (Rosebud.zipWithNEForest (+) x y)
          `shouldBe` Rosebud.zipWithForest (+) (asList x) (asList y)

  describe "pathsForest" do
    it "empty" do
      let forest = []
      Rosebud.pathsForest @Char forest `shouldBe` Nothing
    it "singleton" do
      let forest = [Node 'a' []]
      Rosebud.pathsForest forest
        `shouldBe` Just (NonEmpty.fromList [Sequence.fromList ['a']])
    it "some children" do
      let forest =
            [ Node 'a'
                [ Node 'b'
                    [ Node 'c' []
                    , Node 'd' []
                    ]
                , Node 'e' []
                ]
            , Node 'f' []
            , Node 'g'
                [ Node 'h' []
                ]
            ]
      Rosebud.pathsForest forest
        `shouldBe`
          Just
            ( NonEmpty.fromList
                [ Sequence.fromList "a"
                , Sequence.fromList "ab"
                , Sequence.fromList "abc"
                , Sequence.fromList "abd"
                , Sequence.fromList "ae"
                , Sequence.fromList "f"
                , Sequence.fromList "g"
                , Sequence.fromList "gh"
                ]
            )

  describe "pathsNEForest" do
    it "singleton" do
      let forest = NonEmpty.fromList [Node 'a' []]
      Rosebud.pathsNEForest forest
        `shouldBe` NonEmpty.fromList [Sequence.fromList ['a']]
    it "some children" do
      let forest =
            NonEmpty.fromList
              [ Node 'a'
                  [ Node 'b'
                      [ Node 'c' []
                      , Node 'd' []
                      ]
                  , Node 'e' []
                  ]
              , Node 'f' []
              , Node 'g'
                  [ Node 'h' []
                  ]
              ]
      Rosebud.pathsNEForest forest
        `shouldBe`
          NonEmpty.fromList
            [ Sequence.fromList "a"
            , Sequence.fromList "ab"
            , Sequence.fromList "abc"
            , Sequence.fromList "abd"
            , Sequence.fromList "ae"
            , Sequence.fromList "f"
            , Sequence.fromList "g"
            , Sequence.fromList "gh"
            ]

  describe "leavesForest" do
    it "empty" do
      let forest = []
      Rosebud.leavesForest @Char forest `shouldBe` Nothing
    it "singleton" do
      let forest = [Node 'a' []]
      Rosebud.leavesForest forest
        `shouldBe` Just (NonEmpty.fromList ['a'])
    it "some children" do
      let forest =
            [ Node 'a'
                [ Node 'b'
                    [ Node 'c' []
                    , Node 'd' []
                    ]
                , Node 'e' []
                ]
            , Node 'f' []
            , Node 'g'
                [ Node 'h' []
                ]
            ]
      Rosebud.leavesForest forest
        `shouldBe` Just (NonEmpty.fromList ['c', 'd', 'e', 'f', 'h'])

  describe "leavesNEForest" do
    it "singleton" do
      let forest = NonEmpty.fromList [Node 'a' []]
      Rosebud.leavesNEForest forest
        `shouldBe` NonEmpty.fromList ['a']
    it "some children" do
      let forest =
            NonEmpty.fromList
              [ Node 'a'
                  [ Node 'b'
                      [ Node 'c' []
                      , Node 'd' []
                      ]
                  , Node 'e' []
                  ]
              , Node 'f' []
              , Node 'g'
                  [ Node 'h' []
                  ]
              ]
      Rosebud.leavesNEForest forest
        `shouldBe` NonEmpty.fromList ['c', 'd', 'e', 'f', 'h']

  describe "flattenForest" do
    it "number of labels stays the same" do
      QC.property \(x :: Forest Int) ->
        Foldable.length (Rosebud.flattenForest x)
          `shouldBe` sum (fmap Foldable.length x)
    it "equivalent to concatenating each flattened tree" do
      QC.property \(x :: Forest Int) ->
        Rosebud.flattenForest x `shouldBe` concatMap Tree.flatten x

  describe "flattenNEForest" do
    it "equivalent to 'flattenForest' for non-empty" do
      QC.property \(x :: NEForest Int) ->
        NonEmpty.toList (Rosebud.flattenNEForest x)
          `shouldBe` Rosebud.flattenForest (NonEmpty.toList x)

  describe "singletonTree" do
    it "works" do
      QC.property \(x :: Int) ->
        Rosebud.singletonTree x `shouldBe` Node x []

  describe "subtrees" do
    it "singleton" do
      let tree = Node 'a' []
      Rosebud.subtrees tree `shouldBe` [Node 'a' []]
    it "some children" do
      let tree = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []], Node 'e' []]
      Rosebud.subtrees tree
        `shouldBe`
          [ tree
          , Node 'b' [Node 'c' [], Node 'd' []]
          , Node 'c' []
          , Node 'd' []
          , Node 'e' []
          ]

  describe "neSubtrees" do
    it "is equivalent to 'subtrees' for non-empty" do
      QC.property \(x :: Tree Int) ->
        NonEmpty.toList (Rosebud.neSubtrees x) `shouldBe` Rosebud.subtrees x

  describe "eitherTreeFromLabels" do
    it "returns 'OrphansFoundError when more than one root is present" do
      let root = "1"
      let children =
            [ "1 1"
            , "2"
            ]

      let expectedForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  ]
              ]
      let expectedOrphans = pure "2"

      Rosebud.eitherTreeFromLabels isImmediateChildOf root children
        `shouldBe` Left (Rosebud.OrphansFoundError expectedForest expectedOrphans)
    it "returns 'OrphansFoundError' when orphans are found" do
      let root = "1"
      let children =
            [ "1 1"
            , "1 2 1"
            ]

      let expectedForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  ]
              ]
      let expectedOrphans = pure "1 2 1"

      Rosebud.eitherTreeFromLabels isImmediateChildOf root children
        `shouldBe`
          Left (Rosebud.OrphansFoundError expectedForest expectedOrphans)
    it "builds a tree" do
      let root = "1"
      let children =
            [ "1 1"
            , "1 2"
            , "1 2 1"
            , "1 2 2"
            , "1 3"
            ]
      Rosebud.eitherTreeFromLabels isImmediateChildOf root children
        `shouldBe`
          Right
            ( Node "1"
                [ Node "1 1" []
                , Node "1 2"
                    [ Node "1 2 1" []
                    , Node "1 2 2" []
                    ]
                , Node "1 3" []
                ]
            )

  describe "unsafeTreeFromLabels" do
    it "throws 'OrphansFoundError' when more than one root is present" do
      let root = "1"
      let children =
            [ "1 1"
            , "2"
            ]

      let expectedForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  ]
              ]
      let expectedOrphans = pure "2"

      Exception.evaluate (Rosebud.unsafeTreeFromLabels isImmediateChildOf root children)
        `shouldThrow` orphansFoundErrorSelector expectedForest expectedOrphans
    it "throws 'OrphansFoundError' when orphans are found" do
      let root = "1"
      let children =
            [ "1 1"
            , "1 2 1"
            ]

      let expectedForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  ]
              ]
      let expectedOrphans = pure "1 2 1"

      Exception.evaluate (Rosebud.unsafeTreeFromLabels isImmediateChildOf root children)
        `shouldThrow` orphansFoundErrorSelector expectedForest expectedOrphans
    it "builds a tree" do
      let root = "1"
      let children =
            [ "1 1"
            , "1 2"
            , "1 2 1"
            , "1 2 2"
            , "1 3"
            ]
      Rosebud.unsafeTreeFromLabels isImmediateChildOf root children
        `shouldBe`
            Node "1"
              [ Node "1 1" []
              , Node "1 2"
                  [ Node "1 2 1" []
                  , Node "1 2 2" []
                  ]
              , Node "1 3" []
              ]

  describe "singletonForest" do
    it "works" do
      QC.property \(x :: Int) ->
        Rosebud.singletonForest x `shouldBe` [Node x []]

  describe "singletonForest" do
    it "works" do
      QC.property \(x :: Int) ->
        Rosebud.singletonNEForest x `shouldBe` Node x [] :| []

  describe "eitherNEForestFromPartitionedLabels" do
    it "returns 'OrphansFoundError' when orphans are found" do
      let roots =
            NonEmpty.fromList
              [ "1"
              ]
      let children =
            [ "1 1"
            , "1 2 1"
            ]

      let expectedForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  ]
              ]
      let expectedOrphans = pure "1 2 1"

      Rosebud.eitherNEForestFromPartitionedLabels isImmediateChildOf roots children
        `shouldBe`
          Left (Rosebud.OrphansFoundError expectedForest expectedOrphans)
    it "builds a forest" do
      let roots =
            NonEmpty.fromList
              [ "1"
              , "2"
              ]
      let children =
            [ "1 1"
            , "1 2"
            , "1 2 1"
            , "1 2 2"
            , "1 3"
            , "2 1"
            ]
      let expectedNEForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  , Node "1 2"
                      [ Node "1 2 1" []
                      , Node "1 2 2" []
                      ]
                  , Node "1 3" []
                  ]
              , Node "2"
                  [ Node "2 1" []
                  ]
              ]
      Rosebud.eitherNEForestFromPartitionedLabels isImmediateChildOf roots children
        `shouldBe` Right expectedNEForest

  describe "unsafeNEForestFromPartitionedLabels" do
    it "throws 'OrphansFoundError' when orphans are found" do
      let roots =
            NonEmpty.fromList
              [ "1"
              ]
      let children =
            [ "1 1"
            , "1 2 1"
            ]

      let expectedForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  ]
              ]
      let expectedOrphans = pure "1 2 1"

      Exception.evaluate (Rosebud.unsafeNEForestFromPartitionedLabels isImmediateChildOf roots children)
        `shouldThrow` orphansFoundErrorSelector expectedForest expectedOrphans
    it "builds a forest" do
      let roots =
            NonEmpty.fromList
              [ "1"
              , "2"
              ]
      let children =
            [ "1 1"
            , "1 2"
            , "1 2 1"
            , "1 2 2"
            , "1 3"
            , "2 1"
            ]
      let expectedNEForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  , Node "1 2"
                      [ Node "1 2 1" []
                      , Node "1 2 2" []
                      ]
                  , Node "1 3" []
                  ]
              , Node "2"
                  [ Node "2 1" []
                  ]
              ]
      Rosebud.unsafeNEForestFromPartitionedLabels isImmediateChildOf roots children
        `shouldBe` expectedNEForest

  describe "eitherNEForestFromLabels" do
    it "returns 'NoRootsFoundError' when no roots can be found" do
      let labels =
            NonEmpty.fromList
              [ "1 1"
              , "1 2"
              ]

      Rosebud.eitherNEForestFromLabels isRoot isImmediateChildOf labels
        `shouldBe` Left (Rosebud.NoRootsFoundError labels)
    it "returns 'OrphansFoundError' when orphans are found" do
      let labels =
            NonEmpty.fromList
              [ "1"
              , "1 1"
              , "1 2 1"
              ]

      let expectedForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  ]
              ]
      let expectedOrphans = pure "1 2 1"

      Rosebud.eitherNEForestFromLabels isRoot isImmediateChildOf labels
        `shouldBe`
          Left
            ( Rosebud.FromPartitionedLabels
                ( Rosebud.OrphansFoundError expectedForest expectedOrphans
                )
            )
    it "builds a forest" do
      let labels =
            NonEmpty.fromList
              [ "1"
              , "1 1"
              , "1 2"
              , "1 2 1"
              , "1 2 2"
              , "1 3"
              , "2"
              , "2 1"
              ]
      let expectedNEForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  , Node "1 2"
                      [ Node "1 2 1" []
                      , Node "1 2 2" []
                      ]
                  , Node "1 3" []
                  ]
              , Node "2"
                  [ Node "2 1" []
                  ]
              ]
      Rosebud.eitherNEForestFromLabels isRoot isImmediateChildOf labels
        `shouldBe` Right expectedNEForest

  describe "unsafeNEForestFromLabels" do
    it "throws 'NoRootsFoundError' when no roots can be found" do
      let labels =
            NonEmpty.fromList
              [ "1 1"
              , "1 2"
              ]

      Exception.evaluate (Rosebud.unsafeNEForestFromLabels isRoot isImmediateChildOf labels)
        `shouldThrow` noRootsFoundErrorSelector labels
    it "throws 'OrphansFoundError' when orphans are found" do
      let labels =
            NonEmpty.fromList
              [ "1"
              , "1 1"
              , "1 2 1"
              ]

      let expectedForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  ]
              ]
      let expectedOrphans = pure "1 2 1"

      Exception.evaluate (Rosebud.unsafeNEForestFromLabels isRoot isImmediateChildOf labels)
        `shouldThrow` fromPartitionedLabels'OrphansFoundErrorSelector expectedForest expectedOrphans
    it "builds a forest" do
      let labels =
            NonEmpty.fromList
              [ "1"
              , "1 1"
              , "1 2"
              , "1 2 1"
              , "1 2 2"
              , "1 3"
              , "2"
              , "2 1"
              ]
      let expectedNEForest =
            NonEmpty.fromList
              [ Node "1"
                  [ Node "1 1" []
                  , Node "1 2"
                      [ Node "1 2 1" []
                      , Node "1 2 2" []
                      ]
                  , Node "1 3" []
                  ]
              , Node "2"
                  [ Node "2 1" []
                  ]
              ]
      Rosebud.unsafeNEForestFromLabels isRoot isImmediateChildOf labels
        `shouldBe` expectedNEForest

  describe "neForest" do
    it "when given non-empty forest, produces Just" do
      Rosebud.neForest @(Forest Int) [] `shouldBe` Nothing
    it "when given non-empty forest, produces Just" do
      QC.property \(x :: Forest Int) ->
        not (null x) ==> Rosebud.neForest x `shouldBe` Just (NonEmpty.fromList x)

  describe "unsafeNEForest" do
    it "when given non-empty forest, produces Just" do
      Exception.evaluate (Rosebud.unsafeNEForest @(Forest Int) [])
        `shouldThrow` unsafeNEForestErrorSelector
    it "when given non-empty forest, produces Just" do
      QC.property \(x :: Forest Int) ->
        not (null x) ==> Rosebud.unsafeNEForest x `shouldBe` NonEmpty.fromList x

-------------------------------------------------------------------------------

childrenSortedBy :: (a -> a -> Bool) -> Tree a -> Bool
childrenSortedBy f = \case
  Node { subForest } -> and $ b : bs where
    b = isSortedBy f $ fmap Tree.rootLabel subForest
    bs = fmap (childrenSortedBy f) subForest

isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy f xs = and $ zipWith f xs $ drop 1 xs

-------------------------------------------------------------------------------

isRoot :: String -> Bool
isRoot x = ' ' `notElem` x

isImmediateChildOf :: String -> String -> Bool
isImmediateChildOf x y = init (words x) == words y

-------------------------------------------------------------------------------

unsafeNEForestErrorSelector :: ErrorCall -> Bool
unsafeNEForestErrorSelector = \case
  ErrorCall err -> err == "Rosebud.unsafeNEForest: empty forest"

orphansFoundErrorSelector
  :: (Eq a)
  => NEForest a
  -> NonEmpty a
  -> Rosebud.FromPartitionedLabelsError a
  -> Bool
orphansFoundErrorSelector expectedForest expectedOrphans = \case
  Rosebud.OrphansFoundError actualForest actualOrphans ->
    expectedForest == actualForest && expectedOrphans == actualOrphans

noRootsFoundErrorSelector
  :: (Eq a)
  => NonEmpty a
  -> Rosebud.FromLabelsError a
  -> Bool
noRootsFoundErrorSelector expectedLabels = \case
  Rosebud.NoRootsFoundError actualLabels -> expectedLabels == actualLabels
  _ -> False

fromPartitionedLabels'OrphansFoundErrorSelector
  :: (Eq a)
  => NEForest a
  -> NonEmpty a
  -> Rosebud.FromLabelsError a
  -> Bool
fromPartitionedLabels'OrphansFoundErrorSelector expectedForest expectedOrphans =
  \case
    Rosebud.FromPartitionedLabels
      ( Rosebud.OrphansFoundError actualForest actualOrphans
      ) -> expectedForest == actualForest && expectedOrphans == actualOrphans
    _ -> False
