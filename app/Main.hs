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
        <: #users   @= usersOpt
        <: #teams   @= teamsOpt
        <: #invite  @= inviteOpt
        <: #kick    @= kickOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "users"   >: [Text]
   , "teams"   >: [Text]
   , "invite"  >: Maybe Target
   , "kick"    >: Maybe Target
   ]

data Target = Org | Team

stringToTarget :: String -> Maybe Target
stringToTarget "org"          = Just Org
stringToTarget "organization" = Just Org
stringToTarget "team"         = Just Team
stringToTarget _              = Nothing

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

usersOpt :: OptDescr' [Text]
usersOpt = optTextList [] ["users"] "IDS" "Filter users"

teamsOpt :: OptDescr' [Text]
teamsOpt = optTextList [] ["teams"] "IDS" "Filter teams"

inviteOpt :: OptDescr' (Maybe Target)
inviteOpt = (stringToTarget =<<) <$> optLastArg [] ["invite"] "(org|team)" "Invite user to GitHub Org or Team"

kickOpt :: OptDescr' (Maybe Target)
kickOpt = (stringToTarget =<<) <$> optLastArg [] ["kick"] "(org|team)" "Kick user from GitHub Org or Team"

runCmd :: String -> Options -> Maybe String -> IO ()
runCmd usage opts path =
  case (opts ^. #invite, opts ^. #kick) of
    (Just Org, Nothing)  -> run $ OctBook.inviteOrg (opts ^. #users)
    (Just Team, Nothing) -> run $ OctBook.inviteTeam (opts ^. #teams) (opts ^. #users)
    (Nothing, Just Org)  -> run $ OctBook.kickOrg (opts ^. #users)
    (Nothing, Just Team) -> run $ OctBook.kickTeam (opts ^. #teams) (opts ^. #users)
    _                    -> hPutBuilder stdout ("Please set (invite|kick) option\n" <> fromString usage)
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
    run act = do
      _ <- tryIO $ loadFile defaultConfig
      config <- OctBook.readConfig (fromMaybe ".octbook.yaml" path)
      token  <- liftIO $ fromString <$> getEnv "GH_TOKEN"
      let plugin = hsequence
                 $ #config <@=> pure config
                <: #github <@=> MixGitHub.buildPlugin token
                <: #logger <@=> MixLogger.buildPlugin logOpts
                <: nil
      Mix.run plugin act

optTextList :: [Char] -> [String] -> String -> String -> OptDescr' [Text]
optTextList = optionReqArg (pure . L.concatMap (T.split (== ',') . fromString))
