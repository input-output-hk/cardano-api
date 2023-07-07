{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}

-- | Class of errors used in the Api.
--
module Cardano.Api.Error
  ( Error(..)
  , throwErrorAsException
  , ErrorAsException(..)
  , FileError(..)
  , fileIOExceptT
  ) where

import           Control.Exception (Exception (..), IOException, throwIO)
import           System.IO (Handle)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (handleIOExceptT)
import Control.Monad.IO.Class (MonadIO)
import System.Directory (doesFileExist)
import Control.Monad.Except (throwError)


class Show e => Error e where

    displayError :: e -> String

instance Error () where
    displayError () = ""


-- | The preferred approach is to use 'Except' or 'ExceptT', but you can if
-- necessary use IO exceptions.
--
throwErrorAsException :: Error e => e -> IO a
throwErrorAsException e = throwIO (ErrorAsException e)

data ErrorAsException where
     ErrorAsException :: Error e => e -> ErrorAsException

instance Show ErrorAsException where
    show (ErrorAsException e) = show e

instance Exception ErrorAsException where
    displayException (ErrorAsException e) = displayError e


data FileError e = FileError FilePath e
                 | FileErrorTempFile
                     FilePath
                     -- ^ Target path
                     FilePath
                     -- ^ Temporary path
                     Handle
                 | FileDoesNotExistError FilePath
                 | FileIOError FilePath IOException
  deriving (Show, Eq, Functor)

instance Error e => Error (FileError e) where
  displayError (FileErrorTempFile targetPath tempPath h)=
    "Error creating temporary file at: " ++ tempPath ++
    "/n" ++ "Target path: " ++ targetPath ++
    "/n" ++ "Handle: " ++ show h
  displayError (FileDoesNotExistError path) =
    "Error file not found at: " ++ path
  displayError (FileIOError path ioe) =
    path ++ ": " ++ displayException ioe
  displayError (FileError path e) =
    path ++ ": " ++ displayError e

instance Error IOException where
  displayError = show

fileIOExceptT :: MonadIO m
              => FilePath
              -> (FilePath -> IO s)
              -> ExceptT (FileError e) m s
fileIOExceptT fp readFile' = do
  fileExists <- handleIOExceptT (FileIOError fp) $ doesFileExist fp
  if fileExists then do
                  handleIOExceptT (FileIOError fp) $ readFile' fp
                else throwError (FileDoesNotExistError fp)

