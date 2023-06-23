{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  , EraCastLossy(..)
  , EraCastError(..)
  ) where

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)

import           Data.Kind (Type)

data EraCastError = forall fromEra toEra value.
  ( IsCardanoEra fromEra
  , IsCardanoEra toEra
  , Show value
  ) =>
    EraCastError
    { originalValue :: value
    , fromEra :: CardanoEra fromEra
    , toEra :: CardanoEra toEra
    }

class EraCast (f :: Type -> Type) where
  eraCast :: (IsCardanoEra fromEra, IsCardanoEra toEra)
          => CardanoEra toEra
          -> f fromEra
          -> Either EraCastError (f toEra)

-- | Cast a value from one era to another.  Any data contained within
-- that is not applicable in the target era is dropped.
class EraCastLossy (f :: Type -> Type) where
  eraCastLossy :: ()
    => CardanoEra toEra -- ^ The era to cast to
    -> f fromEra -- ^ The value to cast
    -> f toEra
