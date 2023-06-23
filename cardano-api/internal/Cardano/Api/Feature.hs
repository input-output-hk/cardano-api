{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Feature
  ( FeatureValue (..)
  , FeatureInEra(..)
  , Flip(..)
  , featureInShelleyBasedEra
  , valueOrDefault
  , asFeatureValue
  , asFeatureValueInShelleyBasedEra
  , existsFeatureValue  , eraCastLossyFeatureValue
  , (.:?^)
  ) where

import           Cardano.Api.EraCast
import           Cardano.Api.Eras

import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types
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

-- | A value of type @'FeatureValue' feature era a@ is either:
data FeatureValue feature era a where
  -- | A value is available for this feature in this era
  FeatureValue
    :: feature era
    -- ^ The witness that the feature is supported in this era
    -> a
    -- ^ The value to use
    -> FeatureValue feature era a

  -- | No value is available for this feature in this era
  NoFeatureValue
    :: FeatureValue feature era a

deriving instance (Eq a, Eq (feature era)) => Eq (FeatureValue feature era a)
deriving instance (Show a, Show (feature era)) => Show (FeatureValue feature era a)

instance ToJSON a => ToJSON (FeatureValue feature era a) where
  toJSON v =
    toJSON $
      case v of
        NoFeatureValue -> Nothing
        FeatureValue _ a -> Just a

instance (IsCardanoEra era, FromJSON a, FeatureInEra feature) => FromJSON (FeatureValue feature era a) where
  parseJSON v =
    featureInEra
      (pure NoFeatureValue)
      (\fe -> FeatureValue fe <$> parseJSON v)
      cardanoEra

newtype Flip t a b = Flip { unFlip :: t b a }

instance FeatureInEra feature => EraCastLossy (Flip (FeatureValue feature) a) where
  eraCastLossy era (Flip fv) = Flip $ eraCastLossyFeatureValue era fv

eraCastLossyFeatureValue :: ()
  => FeatureInEra feature
  => CardanoEra toEra
  -> FeatureValue feature fromEra a
  -> FeatureValue feature toEra a
eraCastLossyFeatureValue era fv =
  case fv of
    FeatureValue _ a -> featureInEra NoFeatureValue (\fe -> FeatureValue fe a) era
    NoFeatureValue -> NoFeatureValue


(.:?^) :: (IsCardanoEra era, FromJSON a, FeatureInEra feature) => Object -> Key -> Parser (FeatureValue feature era a)
(.:?^) = explicitParseFieldFeatureValue' parseJSON

-- | Variant of '.:!' with explicit parser function.
explicitParseFieldFeatureValue' :: ()
  => IsCardanoEra era
  => FeatureInEra feature
  => (Value -> Parser a)
  -> Object
  -> Key
  -> Parser (FeatureValue feature era a)
explicitParseFieldFeatureValue' p obj key =
  let era = cardanoEra in
  case KM.lookup key obj of
    Nothing -> pure NoFeatureValue
    Just v ->
      featureInEra
        (fail $ "The field " <> show key <> " is not valid for the era " <> show era <> ".")
        (\fe -> FeatureValue fe <$> p v)
        era

-- | Determine if a value is defined.
--
-- If the value is not defined, it could be because the feature is not supported or
-- because the feature is supported but the value is not available.
existsFeatureValue :: FeatureValue feature era a -> Bool
existsFeatureValue = \case
  NoFeatureValue -> False
  FeatureValue _ _ -> True

-- | Get the value if it is defined, otherwise return the default value.
valueOrDefault :: a -> FeatureValue feature era a -> a
valueOrDefault defaultValue = \case
  NoFeatureValue -> defaultValue
  FeatureValue _ a -> a

-- | Attempt to construct a 'FeatureValue' from a value and era.
-- If the feature is not supported in the era, then 'NoFeatureValue' is returned.
asFeatureValue :: ()
  => FeatureInEra feature
  => a
  -> CardanoEra era
  -> FeatureValue feature era a
asFeatureValue value = featureInEra NoFeatureValue (`FeatureValue` value)

-- | Attempt to construct a 'FeatureValue' from a value and a shelley-based-era.
asFeatureValueInShelleyBasedEra :: ()
  => FeatureInEra feature
  => a
  -> ShelleyBasedEra era
  -> FeatureValue feature era a
asFeatureValueInShelleyBasedEra value = asFeatureValue value . shelleyBasedToCardanoEra
