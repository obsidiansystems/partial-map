{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Data.PartialMap where

import Data.Aeson
import Data.Coerce
import Data.Either
import Data.Maybe
import Data.Map.Monoidal as Map
import Data.Monoid hiding ((<>), First (..))
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

data PartialMap k v
   = PartialMap_Complete (MonoidalMap k v)
   | PartialMap_Partial (MonoidalMap k (First (Maybe v)))
   deriving (Show, Read, Eq, Ord, Foldable, Functor, Generic)

isComplete :: PartialMap k v -> Bool
isComplete = isJust . getComplete

getComplete :: PartialMap k v -> Maybe (MonoidalMap k v)
getComplete = \case
  PartialMap_Complete m -> Just m
  PartialMap_Partial _ -> Nothing

knownKeysSet :: PartialMap k v -> Set k
knownKeysSet = \case
  PartialMap_Complete x -> Map.keysSet x
  PartialMap_Partial x -> Map.keysSet $ Map.mapMaybe getFirst x

knownKeys :: PartialMap k v -> [k]
knownKeys = Set.toList . knownKeysSet

knownSubMap :: PartialMap k v -> MonoidalMap k v
knownSubMap = \case
  PartialMap_Complete m -> m
  PartialMap_Partial m -> Map.mapMaybe getFirst m

instance (Ord k) => Monoid (PartialMap k v) where
  mempty = PartialMap_Partial mempty
  mappend new old = case new of
    PartialMap_Complete _ -> new
    PartialMap_Partial p -> case old of
      PartialMap_Partial oldp -> PartialMap_Partial $ p <> oldp
      PartialMap_Complete oldc -> PartialMap_Complete $ applyMap (coerce p) (coerce oldc)
      where
        applyMap :: Ord k => MonoidalMap k (Maybe v) -> MonoidalMap k v -> MonoidalMap k v
        applyMap patch old' = Map.unionWith const insertions (old' `Map.difference` deletions)
          where (deletions, insertions) = mapPartitionEithers $ maybeToEither <$> patch
                maybeToEither = \case
                  Nothing -> Left ()
                  Just r -> Right r
        mapPartitionEithers :: MonoidalMap k (Either a b) -> (MonoidalMap k a, MonoidalMap k b)
        mapPartitionEithers m = (fromLeft <$> ls, fromRight <$> rs)
          where (ls, rs) = Map.partition isLeft m
                fromLeft (Left l) = l
                fromLeft _ = error "mapPartitionEithers: fromLeft received a Right value; this should be impossible"
                fromRight (Right r) = r
                fromRight _ = error "mapPartitionEithers: fromRight received a Left value; this should be impossible"

instance (Ord k) => Semigroup (PartialMap k v) where
  (<>) = mappend

instance (ToJSONKey k, ToJSON v) => ToJSON (PartialMap k v)
instance (Ord k, FromJSONKey k, FromJSON v) => FromJSON (PartialMap k v)

type PartialSet k = PartialMap k ()

fromKnown :: Set k -> PartialSet k
fromKnown = PartialMap_Partial . Map.fromSet (\_ -> First $ Just ())

fromKnownAbsent :: Set k -> PartialMap k ()
fromKnownAbsent = PartialMap_Partial . Map.fromSet (\_ -> First Nothing)

fromKnownComplete :: Set k -> PartialMap k ()
fromKnownComplete = PartialMap_Complete . Map.fromSet (\_ -> ())
