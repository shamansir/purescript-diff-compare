module Data.Text.Diff.Effectful
    ( compareBy
    , compareByWP
    , compareBy_
    , compareByWP_
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

import Data.Text.Diff (class Diffable, Comparator(..), ComparisonResult(..), Limit(..), Whitespace(..))
import Data.Text.Diff (compareBy_) as Core


compareBy :: forall m. MonadEffect m => MonadThrow Error m => Comparator -> (String -> String -> m Unit)
compareBy = compareBy_


compareByWP :: forall m. MonadEffect m => MonadThrow Error m => Comparator -> (String -> String -> m Unit)
compareByWP = compareByWP_


compareBy_ :: forall m t. Diffable t => MonadEffect m => MonadThrow Error m => Comparator -> (t -> t -> m Unit)
compareBy_ cmp = _effectfulCompare NormalOutput cmp


compareByWP_ :: forall m t. Diffable t => MonadEffect m => MonadThrow Error m => Comparator -> (t -> t -> m Unit)
compareByWP_ cmp = _effectfulCompare HighlightWhitespace cmp


diffCompare
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
diffCompare =
    _effectfulCompare NormalOutput $ Zip NoLimit


diffCompare'
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => Limit
  -> t
  -> t
  -> m Unit
diffCompare' =
  _effectfulCompare NormalOutput <<< Zip


diffStackCompare
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
diffStackCompare =
  _effectfulCompare NormalOutput $ Stack NoLimit


diffStackCompare'
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => Limit
  -> t
  -> t
  -> m Unit
diffStackCompare' =
  _effectfulCompare NormalOutput <<< Stack


onlyDifferentCompare
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
onlyDifferentCompare =
  _effectfulCompare NormalOutput $ OnlyDifferent NoLimit


onlyDifferentCompare'
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => Limit
  -> t
  -> t
  -> m Unit
onlyDifferentCompare' =
  _effectfulCompare NormalOutput <<< OnlyDifferent


_effectfulCompare :: forall m t. MonadEffect m => MonadThrow Error m => Diffable t => Whitespace -> Comparator -> t -> t -> m Unit
_effectfulCompare wp cmp =
  let compareFn = Core.compareBy_ wp cmp
  in \v1 v2 ->
    case compareFn v1 v2 of
      ThingsEqual -> pure unit
      ThingsMismatch { diff } -> liftEffect $ throw diff
