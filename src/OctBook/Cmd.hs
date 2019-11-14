module OctBook.Cmd where

import           RIO

import           OctBook.Env

cmd :: RIO Env ()
cmd = showNotImpl

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
