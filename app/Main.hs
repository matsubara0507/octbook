module Main where

import           Paths_octbook          (version)
import           RIO
import qualified RIO.List               as L
import qualified RIO.Text               as T

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           GetOpt                 (withGetOpt')
import qualified Mix
import qualified Mix.Plugin.GitHub      as MixGitHub
import           Mix.Plugin.Logger      as MixLogger
import qualified OctBook
import           System.Environment     (getEnv)
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version)
     | otherwise     -> runCmd usage r (listToMaybe args)
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #config  @= configOpt
        <: #users   @= usersOpt
        <: #teams   @= teamsOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "config"  >: Maybe FilePath
   , "users"   >: [Text]
   , "teams"   >: [Text]
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

configOpt :: OptDescr' (Maybe FilePath)
configOpt = optLastArg ['c'] ["config"] "PATH" "Configuration file path"

usersOpt :: OptDescr' [Text]
usersOpt = optionReqArg (pure . L.concatMap (T.split (== ',') . fromString)) [] ["users"] "IDS" "Filter users"

teamsOpt :: OptDescr' [Text]
teamsOpt = optionReqArg (pure . L.concatMap (T.split (== ',') . fromString)) [] ["teams"] "IDS" "Filter teams"

runCmd :: String -> Options -> Maybe String -> IO ()
runCmd usage opts subcmd =
  case subcmd of
    Just "invite-org"  -> run $ OctBook.inviteOrg (opts ^. #users)
    Just "invite-team" -> run $ OctBook.inviteTeam (opts ^. #users) (opts ^. #teams)
    _                  -> hPutBuilder stdout (fromString usage)
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
    run act = do
      _ <- tryIO $ loadFile defaultConfig
      config <- OctBook.readConfig (fromMaybe ".octbook.yaml" $ opts ^. #config)
      token  <- liftIO $ fromString <$> getEnv "GH_TOKEN"
      let plugin = hsequence
                 $ #config <@=> pure config
                <: #github <@=> MixGitHub.buildPlugin token
                <: #logger <@=> MixLogger.buildPlugin logOpts
                <: nil
      Mix.run plugin act
