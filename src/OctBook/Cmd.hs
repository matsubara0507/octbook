module OctBook.Cmd
  ( inviteOrg
  , inviteTeam
  ) where

import           RIO
import qualified RIO.List                               as L

import           Data.Fallible
import           GitHub.Data.Name                       (mkName)
import qualified GitHub.Endpoints.Organizations.Members as GitHub
import qualified GitHub.Endpoints.Organizations.Teams   as GitHub
import qualified Mix.Plugin.GitHub                      as MixGitHub
import           OctBook.Config                         (User)
import           OctBook.Env

inviteOrg :: [Text] -> RIO Env ()
inviteOrg = actForUsers inviteUserToGitHubOrg

inviteTeam :: [Text] -> [Text] -> RIO Env ()
inviteTeam teamIDs = actForUsers (inviteUserToGitHubOrgTeam teamIDs)

actForUsers :: (User -> RIO Env ()) -> [Text] -> RIO Env ()
actForUsers act userIDs = do
  users <- asks (view #config)
  if | null userIDs -> mapM_ act users
     | otherwise    -> mapUsersWithFilterByIDs userIDs act users

mapUsersWithFilterByIDs :: [Text] -> (User -> RIO Env ()) -> [User] -> RIO Env ()
mapUsersWithFilterByIDs userIDs act users =
  forM_ userIDs $ \idx ->
    case L.find (\user -> user ^. #id == idx) users of
      Just user -> act user
      Nothing   -> logWarn (display $ "user not found: " <> idx)

inviteUserToGitHubOrg :: User -> RIO Env ()
inviteUserToGitHubOrg user = do
  resp <- MixGitHub.fetch $ \auth -> GitHub.addOrUpdateMembership' auth
    (mkName Proxy $ user ^. #org)
    (mkName Proxy $ user ^. #id)
    False
  logAct "invite" ("to " <> user ^. #org) user resp

inviteUserToGitHubOrgTeam :: [Text] -> User -> RIO Env ()
inviteUserToGitHubOrgTeam teamIDs user =
  forM_ (L.intersect teamIDs' $ user ^. #teams) $ \teamId -> evalContT $ do
    resp <- MixGitHub.fetch $ \auth -> GitHub.teamInfoByName' (Just auth)
      (mkName Proxy $ user ^. #org)
      (mkName Proxy teamId)
    team <- resp ??= exit . logAct "invite" (target teamId) user . Left
    resp' <- MixGitHub.fetch $ \auth -> GitHub.addTeamMembershipFor' auth
      (GitHub.teamId team)
      (mkName Proxy $ user ^. #id)
      GitHub.RoleMember
    lift $ logAct "invite" (target teamId) user resp'
  where
    teamIDs' = if null teamIDs then user ^. #teams else teamIDs
    target teamId = "to " <> user ^. #org <> ":" <> teamId

logAct :: Show e => Text -> Text -> User -> Either e a -> RIO Env ()
logAct act target user = \case
  Right _  -> logInfo (display successMessage)
  Left err -> logDebug (displayShow err) >> logWarn (display failMessage)
  where
    successMessage = mconcat
      [ "Success: ", act, " "
      , user ^. #name, "(", user ^. #id, ")", " ", target, "."
      ]
    failMessage = mconcat
      [ "Fail: ", act, " ", user ^. #id, " ", target, "."
      ]
