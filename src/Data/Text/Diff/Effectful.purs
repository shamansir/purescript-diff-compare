module Data.Text.Diff.Effectful
    ( compareBy
    , diffCompare
    , diffCompare'
    , diffStackCompare
    , diffStackCompare'
    , onlyDifferentCompare
    , onlyDifferentCompare'
    )
    where

import Prelude


import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)

import Control.Monad.Error.Class (class MonadThrow)

import Data.Text.Diff (class Diffable, Comparator(..), Limit(..), lineByLineComparison, onlyDiffsComparison, toDiffString, twoStacksComparison)


compareBy :: forall m. MonadEffect m => MonadThrow Error m => Comparator -> (String -> String -> m Unit)
compareBy (Stack limit) = diffStackCompare' limit
compareBy (Zip limit) = diffCompare' limit
compareBy (OnlyDifferent limit) = onlyDifferentCompare' limit
compareBy Plain  = \sA sB -> when (sA /= sB) $ liftEffect $ throw $ show sA <> " ≠ " <> show sB
compareBy Silent = \sA sB -> when (sA /= sB) $ liftEffect $ throw "x"


diffCompare
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
diffCompare =
    diffCompare' NoLimit


diffCompare'
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => Limit
  -> t
  -> t
  -> m Unit
diffCompare' limit v1 v2 =
  when (v1 /= v2) $
    liftEffect $ throw $ lineByLineComparison limit (toDiffString v1) (toDiffString v2)


diffStackCompare
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
diffStackCompare =
  diffStackCompare' NoLimit


diffStackCompare'
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => Limit
  -> t
  -> t
  -> m Unit
diffStackCompare' limit v1 v2 =
  when (v1 /= v2) $
    liftEffect $ throw $ twoStacksComparison limit (toDiffString v1) (toDiffString v2)


onlyDifferentCompare
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
onlyDifferentCompare =
  onlyDifferentCompare' NoLimit


onlyDifferentCompare'
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => Limit
  -> t
  -> t
  -> m Unit
onlyDifferentCompare' limit v1 v2 =
  when (v1 /= v2) $
    liftEffect $ throw $ onlyDiffsComparison limit (toDiffString v1) (toDiffString v2)


