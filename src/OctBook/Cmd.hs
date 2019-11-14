module OctBook.Cmd
  ( inviteOrg
  , inviteTeam
  ) where

import           RIO

import           OctBook.Env

inviteOrg :: [Text] -> RIO Env ()
inviteOrg _ = showNotImpl

inviteTeam :: [Text] -> [Text] -> RIO Env ()
inviteTeam _ _ = showNotImpl

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
