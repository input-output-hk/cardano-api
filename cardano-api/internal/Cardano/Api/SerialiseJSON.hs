{-# LANGUAGE DeriveDataTypeable #-}

-- | JSON serialisation
--
module Cardano.Api.SerialiseJSON
  ( serialiseToJSON
  , ToJSON(..)
  , ToJSONKey
  , deserialiseFromJSON
  , prettyPrintJSON
  , FromJSON(..)
  , FromJSONKey
  , JsonDecodeError(..)
  , readFileJSON
  , writeFileJSON
  ) where

import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import           Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Data (Data)


newtype JsonDecodeError = JsonDecodeError String
  deriving (Eq, Show, Data)

instance Error JsonDecodeError where
  displayError (JsonDecodeError err) = err


serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

prettyPrintJSON :: ToJSON a => a -> ByteString
prettyPrintJSON = LBS.toStrict . encodePretty

deserialiseFromJSON :: FromJSON a
                    => AsType a
                    -> ByteString
                    -> Either JsonDecodeError a
deserialiseFromJSON _proxy = either (Left . JsonDecodeError) Right
                           . Aeson.eitherDecodeStrict'


readFileJSON :: FromJSON a
             => AsType a
             -> FilePath
             -> IO (Either (FileError JsonDecodeError) a)
readFileJSON ttoken path =
    runExceptT $ do
      content <- fileIOExceptT path BS.readFile
      firstExceptT (FileError path) $ hoistEither $
        deserialiseFromJSON ttoken content

writeFileJSON :: ToJSON a
              => FilePath
              -> a
              -> IO (Either (FileError ()) ())
writeFileJSON path x =
    runExceptT $
      fileIOExceptT path (`BS.writeFile` serialiseToJSON x)

