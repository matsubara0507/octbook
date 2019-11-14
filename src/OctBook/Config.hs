module OctBook.Config where

import           RIO

import           Data.Extensible
import qualified Data.Yaml       as Y

type Config = [User]

type User = Record
  '[ "id"    >: Text   -- GitHub Account
   , "name"  >: Text
   , "org"   >: Text   -- GitHub Org
   , "teams" >: [Text] -- Team in GitHub Org
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow
