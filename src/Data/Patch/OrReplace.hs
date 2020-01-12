{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- The derived instances are undecidable in the case of a pathological instance like
-- instance Patch x where
--   type PatchTarget x = PatchOrReplace x
{-# LANGUAGE UndecidableInstances #-}

module Data.Patch.OrReplace where

--import Data.Aeson
import Data.Coerce
import Data.Either
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid hiding ((<>), First (..))
import Data.Patch
import Data.Patch.Map
import Data.Semigroup
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics

-- | A choice of either a patch or a replacement value.
data PatchOrReplace p
  = PatchOrReplace_Patch p
  | PatchOrReplace_Complete (PatchTarget p)
  deriving (Generic)

type PartialMap k v = PatchOrReplace (PatchMap k v)

isComplete :: PatchOrReplace p -> Bool
isComplete = isJust . getComplete

getComplete :: PatchOrReplace p -> Maybe (PatchTarget p)
getComplete = \case
  PatchOrReplace_Patch _ -> Nothing
  PatchOrReplace_Complete m -> Just m

knownKeysSet :: PatchOrReplace (PatchMap k v) -> Set k
knownKeysSet = \case
  PatchOrReplace_Complete x -> Map.keysSet x
  PatchOrReplace_Patch x -> Map.keysSet $ Map.mapMaybe id $ unPatchMap x

completePatchOrReplace :: PatchOrReplace p -> Maybe (PatchTarget p)
completePatchOrReplace = \case
  PatchOrReplace_Complete t -> Just t
  PatchOrReplace_Patch _ -> Nothing

knownKeys :: PartialMap k v -> [k]
knownKeys = Set.toList . knownKeysSet

knownSubMap :: PartialMap k v -> Map k v
knownSubMap = \case
  PatchOrReplace_Complete m -> m
  PatchOrReplace_Patch m -> Map.mapMaybe id $ unPatchMap m

deriving instance (Eq p, Eq (PatchTarget p)) => Eq (PatchOrReplace p)
deriving instance (Ord p, Ord (PatchTarget p)) => Ord (PatchOrReplace p)
deriving instance (Show p, Show (PatchTarget p)) => Show (PatchOrReplace p)
deriving instance (Read p, Read (PatchTarget p)) => Read (PatchOrReplace p)
-- instance (ToJSON p, ToJSON (PatchTarget p)) => ToJSON (PatchOrReplace p)
-- instance (FromJSON p, FromJSON (PatchTarget p)) => FromJSON (PatchOrReplace p)

instance (Monoid p, Patch p) => Monoid (PatchOrReplace p) where
  mempty = PatchOrReplace_Patch mempty
  mappend = (<>)

instance (Semigroup p, Patch p) => Semigroup (PatchOrReplace p) where
  (<>) = curry $ \case
    (PatchOrReplace_Patch a, PatchOrReplace_Patch b) -> PatchOrReplace_Patch $ a <> b
    (PatchOrReplace_Patch a, PatchOrReplace_Complete b) -> PatchOrReplace_Complete $ applyAlways a b
    (PatchOrReplace_Complete a, _) -> PatchOrReplace_Complete a

instance (PatchHet p) => PatchHet (PatchOrReplace p) where
  type PatchTarget (PatchOrReplace p) = PatchTarget p
  type PatchSource (PatchOrReplace p) = PatchSource p
  applyHet p0 m = case p0 of
    PatchOrReplace_Patch p -> applyHet p m
    PatchOrReplace_Complete c -> Right c
instance (Patch p) => Patch (PatchOrReplace p) where
  apply p0 m = case p0 of
    PatchOrReplace_Patch p -> apply p m
    PatchOrReplace_Complete c -> Just c

--instance (ToJSONKey k, ToJSON v) => ToJSON (PartialMap k v)
--instance (Ord k, FromJSONKey k, FromJSON v) => FromJSON (PatchOrReplace (PatchMap k v))

type PartialSet k = PartialMap k ()

fromKnown :: Set k -> PartialSet k
fromKnown = PatchOrReplace_Patch . PatchMap . Map.fromSet (\_ -> Just ())

fromKnownAbsent :: Set k -> PartialSet k
fromKnownAbsent = PatchOrReplace_Patch . PatchMap . Map.fromSet (\_ -> Nothing)

fromKnownComplete :: Set k -> PartialSet k
fromKnownComplete = PatchOrReplace_Complete . Map.fromSet (\_ -> ())
