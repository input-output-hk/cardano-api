{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | Internal utils for the other Api modules
--
module Cardano.Api.Utils
  ( (?!)
  , (?!.)
  , formatParsecError
  , failEither
  , failEitherWith
  , noInlineMaybeToStrictMaybe
  , note
  , parseFilePath
  , readFileBlocking
  , renderEra
  , runParsecParser
  , textShow

    -- ** CLI option parsing
  , bounded

    -- ** Constraint solvers
  , obtainCertificateConstraints
  , obtainCryptoConstraints
  , obtainEraConstraints
  , obtainEraPParamsConstraint
  , obtainEraCryptoConstraints
  , obtainSafeToHashConstraint
  ) where

import           Cardano.Api.Eras

import           Cardano.Crypto.Hash.Class (HashAlgorithm)
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import qualified Cardano.Ledger.Crypto as Ledger
import           Cardano.Ledger.Shelley ()
import qualified Cardano.Ledger.Shelley.TxCert as Shelley

import           Control.Exception (bracket)
import           Control.Monad (when)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe.Strict
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.IO.Handle.FD (openFileBlocking)
import           Options.Applicative (ReadM)
import qualified Options.Applicative as Opt
import           Options.Applicative.Builder (eitherReader)
import           System.IO (IOMode (ReadMode), hClose)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.ParserCombinators.Parsec.Error as Parsec
import qualified Text.Read as Read


(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x  ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!. f = Left (f e)
Right x ?!. _ = Right x

{-# NOINLINE noInlineMaybeToStrictMaybe #-}
noInlineMaybeToStrictMaybe :: Maybe a -> StrictMaybe a
noInlineMaybeToStrictMaybe Nothing = SNothing
noInlineMaybeToStrictMaybe (Just x) = SJust x

formatParsecError :: Parsec.ParseError -> String
formatParsecError err =
  Parsec.showErrorMessages "or" "unknown parse error"
    "expecting" "unexpected" "end of input"
    $ Parsec.errorMessages err

runParsecParser :: Parsec.Parser a -> Text -> Aeson.Parser a
runParsecParser parser input =
  case Parsec.parse (parser <* Parsec.eof) "" (Text.unpack input) of
    Right txin -> pure txin
    Left parseError -> fail $ formatParsecError parseError

failEither :: MonadFail m => Either String a -> m a
failEither = either fail pure

failEitherWith :: MonadFail m => (e -> String) -> Either e a -> m a
failEitherWith f = either (fail . f) pure

note :: MonadFail m => String -> Maybe a -> m a
note msg = \case
  Nothing -> fail msg
  Just a -> pure a

parseFilePath :: String -> String -> Opt.Parser FilePath
parseFilePath optname desc =
  Opt.strOption
    ( Opt.long optname
    <> Opt.metavar "FILEPATH"
    <> Opt.help desc
    <> Opt.completer (Opt.bashCompleter "file")
    )

readFileBlocking :: FilePath -> IO BS.ByteString
readFileBlocking path = bracket
  (openFileBlocking path ReadMode)
  hClose
  (\fp -> do
    -- An arbitrary block size.
    let blockSize = 4096
    let go acc = do
          next <- BS.hGet fp blockSize
          if BS.null next
          then pure acc
          else go (acc <> Builder.byteString next)
    contents <- go mempty
    pure $ LBS.toStrict $ Builder.toLazyByteString contents)

textShow :: Show a => a -> Text
textShow = Text.pack . show

renderEra :: AnyCardanoEra -> Text
renderEra (AnyCardanoEra ByronEra)   = "Byron"
renderEra (AnyCardanoEra ShelleyEra) = "Shelley"
renderEra (AnyCardanoEra AllegraEra) = "Allegra"
renderEra (AnyCardanoEra MaryEra)    = "Mary"
renderEra (AnyCardanoEra AlonzoEra)  = "Alonzo"
renderEra (AnyCardanoEra BabbageEra) = "Babbage"
renderEra (AnyCardanoEra ConwayEra)  = "Conway"

bounded :: forall a. (Bounded a, Integral a, Show a) => String -> ReadM a
bounded t = eitherReader $ \s -> do
  i <- Read.readEither @Integer s
  when (i < fromIntegral (minBound @a)) $ Left $ t <> " must not be less than " <> show (minBound @a)
  when (i > fromIntegral (maxBound @a)) $ Left $ t <> " must not greater than " <> show (maxBound @a)
  pure (fromIntegral i)

obtainEraCryptoConstraints
  :: ShelleyBasedEra era
  -> ((EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto) => a)
  -> a
obtainEraCryptoConstraints ShelleyBasedEraShelley f = f
obtainEraCryptoConstraints ShelleyBasedEraAllegra f = f
obtainEraCryptoConstraints ShelleyBasedEraMary    f = f
obtainEraCryptoConstraints ShelleyBasedEraAlonzo  f = f
obtainEraCryptoConstraints ShelleyBasedEraBabbage f = f
obtainEraCryptoConstraints ShelleyBasedEraConway  f = f

obtainCryptoConstraints
  :: ShelleyBasedEra era
  -> ((Crypto (EraCrypto (ShelleyLedgerEra era))) => a)
  -> a
obtainCryptoConstraints ShelleyBasedEraShelley f = f
obtainCryptoConstraints ShelleyBasedEraAllegra f = f
obtainCryptoConstraints ShelleyBasedEraMary    f = f
obtainCryptoConstraints ShelleyBasedEraAlonzo  f = f
obtainCryptoConstraints ShelleyBasedEraBabbage f = f
obtainCryptoConstraints ShelleyBasedEraConway  f = f


obtainEraPParamsConstraint
  :: ShelleyBasedEra era
  -> (Ledger.EraPParams (ShelleyLedgerEra era) => a)
  -> a
obtainEraPParamsConstraint ShelleyBasedEraShelley f = f
obtainEraPParamsConstraint ShelleyBasedEraAllegra f = f
obtainEraPParamsConstraint ShelleyBasedEraMary    f = f
obtainEraPParamsConstraint ShelleyBasedEraAlonzo  f = f
obtainEraPParamsConstraint ShelleyBasedEraBabbage f = f
obtainEraPParamsConstraint ShelleyBasedEraConway  f = f

obtainEraConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> ( (IsShelleyBasedEra era, Ledger.Era ledgerera) => a) -> a
obtainEraConstraints ShelleyBasedEraShelley f = f
obtainEraConstraints ShelleyBasedEraAllegra f = f
obtainEraConstraints ShelleyBasedEraMary    f = f
obtainEraConstraints ShelleyBasedEraAlonzo  f = f
obtainEraConstraints ShelleyBasedEraBabbage f = f
obtainEraConstraints ShelleyBasedEraConway  f = f


obtainSafeToHashConstraint
  :: ShelleyBasedEra era
  -> (HashAlgorithm (Ledger.HASH (EraCrypto (ShelleyLedgerEra era))) => a)
  -> a
obtainSafeToHashConstraint ShelleyBasedEraShelley f = f
obtainSafeToHashConstraint ShelleyBasedEraAllegra f = f
obtainSafeToHashConstraint ShelleyBasedEraMary    f = f
obtainSafeToHashConstraint ShelleyBasedEraAlonzo  f = f
obtainSafeToHashConstraint ShelleyBasedEraBabbage f = f
obtainSafeToHashConstraint ShelleyBasedEraConway  f = f
obtainCertificateConstraints

  :: ShelleyBasedEra era
  -> (( Shelley.ShelleyEraTxCert (ShelleyLedgerEra era)
      , EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
      ) => a)
  -> a
obtainCertificateConstraints ShelleyBasedEraShelley f = f
obtainCertificateConstraints ShelleyBasedEraAllegra f = f
obtainCertificateConstraints ShelleyBasedEraMary    f = f
obtainCertificateConstraints ShelleyBasedEraAlonzo  f = f
obtainCertificateConstraints ShelleyBasedEraBabbage f = f
obtainCertificateConstraints ShelleyBasedEraConway  f = f
