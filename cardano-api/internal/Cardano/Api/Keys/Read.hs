{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Keys.Read
  ( readKeyFile
  , readKeyFileTextEnvelope
  , readKeyFileAnyOf
  ) where

import           Cardano.Api.DeserialiseAnyOf
import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.IO
import           Cardano.Api.SerialiseBech32
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.Utils

import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty)
import Control.Monad.Except (runExceptT)

-- | Read a cryptographic key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readKeyFile
  :: AsType a
  -> NonEmpty (InputFormat a)
  -> FilePath
  -> IO (Either (FileError InputDecodeError) a)
readKeyFile asType acceptedFormats path = do
  eContent <- runExceptT $ fileIOExceptT path readFileBlocking
  case eContent of
    Left e -> return $ Left e
    Right content ->
      return . first (FileError path) $ deserialiseInput asType acceptedFormats content

-- | Read a cryptographic key from a file.
--
-- The contents of the file must be in the text envelope format.
readKeyFileTextEnvelope
  :: HasTextEnvelope a
  => AsType a
  -> File content In
  -> IO (Either (FileError InputDecodeError) a)
readKeyFileTextEnvelope asType fp =
    first (fmap InputTextEnvelopeError) <$> readFileTextEnvelope asType fp

-- | Read a cryptographic key from a file given that it is one of the provided
-- types.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readKeyFileAnyOf
  :: forall content b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> File content In
  -> IO (Either (FileError InputDecodeError) b)
readKeyFileAnyOf bech32Types textEnvTypes path = do
  eContent <- runExceptT $ fileIOExceptT (unFile path) readFileBlocking
  case eContent of
    Left e -> return $ Left e
    Right content ->
      return . first (FileError (unFile path)) $ deserialiseInputAnyOf bech32Types textEnvTypes content
