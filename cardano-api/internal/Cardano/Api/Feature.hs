{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Feature
  ( FeatureValue (..)
  , FeatureInEra(..)
  , featureInShelleyBasedEra
  , valueOrDefault
  , asFeatureValue
  , asFeatureValueInShelleyBasedEra
  , existsFeatureValue
  ) where

import           Cardano.Api.Eras

import           Data.Kind

-- | A class for features that are supported in some eras but not others.
class FeatureInEra (feature :: Type -> Type) where
  -- | Determine the value to use for a feature in a given 'CardanoEra'.
  -- Note that the negative case is the first argument, and the positive case is the second as per
  -- the 'either' function convention.
  featureInEra :: ()
    => a                    -- ^ Value to use if the feature is not supported in the era
    -> (feature era -> a)   -- ^ Function to get thealue to use if the feature is supported in the era
    -> CardanoEra era       -- ^ Era to check
    -> a                    -- ^ The value to use

-- | Determine the value to use for a feature in a given 'ShelleyBasedEra'.
featureInShelleyBasedEra :: ()
  => FeatureInEra feature
  => a
  -> (feature era -> a)
  -> ShelleyBasedEra era
  -> a
featureInShelleyBasedEra no yes = featureInEra no yes . shelleyBasedToCardanoEra

-- | A value of type @'FeatureValue' a feature era@ is either:
data FeatureValue a feature era where
  -- | A value is available for this feature in this era
  FeatureValue
    :: a
    -- ^ The value to use
    -> feature era
    -- ^ The witness that the feature is supported in this era
    -> FeatureValue a feature era

  -- | No value is available for this feature in this era
  NoFeatureValue
    :: FeatureValue a feature era

deriving instance (Eq a, Eq (feature era)) => Eq (FeatureValue a feature era)
deriving instance (Show a, Show (feature era)) => Show (FeatureValue a feature era)

-- | Determine if a value is defined.
--
-- If the value is not defined, it could be because the feature is not supported or
-- because the feature is supported but the value is not available.
existsFeatureValue :: FeatureValue a feature era -> Bool
existsFeatureValue = \case
  NoFeatureValue -> False
  FeatureValue _ _ -> True

-- | Get the value if it is defined, otherwise return the default value.
valueOrDefault :: a -> FeatureValue a feature era -> a
valueOrDefault defaultValue = \case
  NoFeatureValue -> defaultValue
  FeatureValue a _ -> a

-- | Attempt to construct a 'FeatureValue' from a value and era.
-- If the feature is not supported in the era, then 'NoFeatureValue' is returned.
asFeatureValue :: ()
  => FeatureInEra feature
  => a
  -> CardanoEra era
  -> FeatureValue a feature era
asFeatureValue value = featureInEra NoFeatureValue (FeatureValue value)

-- | Attempt to construct a 'FeatureValue' from a value and a shelley-based-era.
asFeatureValueInShelleyBasedEra :: ()
  => FeatureInEra feature
  => a
  -> ShelleyBasedEra era
  -> FeatureValue a feature era
asFeatureValueInShelleyBasedEra value = asFeatureValue value . shelleyBasedToCardanoEra
