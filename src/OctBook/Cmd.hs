module OctBook.Cmd
  ( inviteOrg
  , inviteTeam
  ) where

import           RIO
import qualified RIO.List                               as L

import           GitHub.Data.Name                       (mkName)
import qualified GitHub.Endpoints.Organizations.Members as GitHub
import qualified Mix.Plugin.GitHub                      as MixGitHub
import           OctBook.Config                         (User)
import           OctBook.Env

inviteOrg :: [Text] -> RIO Env ()
inviteOrg userIDs = do
  users <- asks (view #config)
  forM_ userIDs $ \idx ->
    case L.find (\user -> user ^. #id == idx) users of
      Just user -> inviteUserToGitHubOrg user
      Nothing   -> logWarn (display $ "user not found: " <> idx)

inviteTeam :: [Text] -> [Text] -> RIO Env ()
inviteTeam _ _ = showNotImpl

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

inviteUserToGitHubOrg :: User -> RIO Env ()
inviteUserToGitHubOrg user = do
  resp <- MixGitHub.fetch $ \auth -> GitHub.addOrUpdateMembership' auth
    (mkName Proxy $ user ^. #org)
    (mkName Proxy $ user ^. #id)
    False
  case resp of
    Left err -> logDebug (displayShow err) >> logWarn (display failure)
    Right _  -> logInfo $ display success
  where
    failure = "fail invite user: " <> user ^. #id
    success = mconcat
      [ "Success: invite "
      , user ^. #name, "(", user ^. #id, ")", " to ", user ^. #org, "."
      ]
