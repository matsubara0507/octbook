module OctBook.Env where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub as GitHub
import           OctBook.Config

type Env = Record
  '[ "config" >: Config
   , "github" >: GitHub.Token
   , "logger" >: LogFunc
   ]
