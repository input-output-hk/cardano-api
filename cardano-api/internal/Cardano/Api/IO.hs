{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.IO
  ( readByteStringFile
  , readLazyByteStringFile
  , readTextFile

  , writeByteStringFileWithOwnerPermissions
  , writeByteStringFile
  , writeByteStringOutput

  , writeLazyByteStringFileWithOwnerPermissions
  , writeLazyByteStringFile
  , writeLazyByteStringOutput

  , writeTextFileWithOwnerPermissions
  , writeTextFile
  , writeTextOutput

  , File(..)
  , FileDirection(..)
  , SocketPath

  , mapFile
  , onlyIn
  , onlyOut

  , intoFile

  , checkVrfFilePermissions
  , writeSecrets
  ) where

import           Cardano.Api.Error (FileError (..), fileIOExceptT)
import           Cardano.Api.IO.Base
import           Cardano.Api.IO.Compat

import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as LBSC
import           Data.Text (Text)
import qualified Data.Text.IO as Text

readByteStringFile :: ()
  => MonadIO m
  => File content In
  -> m (Either (FileError e) ByteString)
readByteStringFile fp = runExceptT $
  fileIOExceptT (unFile fp) BS.readFile

readLazyByteStringFile :: ()
  => MonadIO m
  => File content In
  -> m (Either (FileError e) LBS.ByteString)
readLazyByteStringFile fp = runExceptT $
  fileIOExceptT (unFile fp) LBS.readFile

readTextFile :: ()
  => MonadIO m
  => File content In
  -> m (Either (FileError e) Text)
readTextFile fp = runExceptT $
  fileIOExceptT (unFile fp) Text.readFile

writeByteStringFile :: ()
  => MonadIO m
  => File content Out
  -> ByteString
  -> m (Either (FileError e) ())
writeByteStringFile fp bs = runExceptT $
  fileIOExceptT (unFile fp) (`BS.writeFile` bs)

writeByteStringFileWithOwnerPermissions
  :: FilePath
  -> BS.ByteString
  -> IO (Either (FileError e) ())
writeByteStringFileWithOwnerPermissions fp bs =
  handleFileForWritingWithOwnerPermission fp $ \h ->
    BS.hPut h bs

writeByteStringOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> ByteString
  -> m (Either (FileError e) ())
writeByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> fileIOExceptT (unFile fp) (`BS.writeFile` bs)
    Nothing -> liftIO $ BSC.putStr bs

writeLazyByteStringFile :: ()
  => MonadIO m
  => File content Out
  -> LBS.ByteString
  -> m (Either (FileError e) ())
writeLazyByteStringFile fp bs = runExceptT $
  fileIOExceptT (unFile fp) (`LBS.writeFile` bs)

writeLazyByteStringFileWithOwnerPermissions
  :: File content Out
  -> LBS.ByteString
  -> IO (Either (FileError e) ())
writeLazyByteStringFileWithOwnerPermissions fp lbs =
  handleFileForWritingWithOwnerPermission (unFile fp) $ \h ->
    LBS.hPut h lbs

writeLazyByteStringOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> LBS.ByteString
  -> m (Either (FileError e) ())
writeLazyByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> fileIOExceptT (unFile fp) (`LBS.writeFile` bs)
    Nothing -> liftIO $ LBSC.putStr bs

writeTextFile :: ()
  => MonadIO m
  => File content Out
  -> Text
  -> m (Either (FileError e) ())
writeTextFile fp t = runExceptT $
  fileIOExceptT (unFile fp) (`Text.writeFile` t)

writeTextFileWithOwnerPermissions
  :: File content Out
  -> Text
  -> IO (Either (FileError e) ())
writeTextFileWithOwnerPermissions fp t =
  handleFileForWritingWithOwnerPermission (unFile fp) $ \h ->
    Text.hPutStr h t

writeTextOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> Text
  -> m (Either (FileError e) ())
writeTextOutput mOutput t = runExceptT $
  case mOutput of
    Just fp -> fileIOExceptT (unFile fp) (`Text.writeFile` t)
    Nothing -> liftIO $ Text.putStr t

mapFile :: (FilePath -> FilePath) -> File content direction -> File content direction
mapFile f = File . f . unFile

onlyIn :: File content InOut -> File content In
onlyIn = File . unFile

onlyOut :: File content InOut -> File content Out
onlyOut = File . unFile

-- | Given a way to serialise a value and a way to write the stream to a file, serialise
-- a value into a stream, and write it to a file.
--
-- Whilst it is possible to call the serialisation and writing functions separately,
-- doing so means the compiler is unable to match the content type of the file with
-- the type of the content being serialised.
--
-- Using this function ensures that the content type of the file always matches with the
-- content value and prevents any type mismatches.
intoFile :: ()
  => File content 'Out
  -> content
  -> (File content 'Out -> stream -> result)
  -> (content -> stream)
  -> result
intoFile fp content write serialise = write fp (serialise content)
