-- Code for the following blog post:
-- https://kodimensional.dev/cps

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module InterfaceAdapters.CPS where

data AppError
    = UserSessionIsInvalid
    | DbError InternalDbError
    | UserAlreadyHasEmail
    | UserHasDifferentEmail
    | EmailIsTaken

data InternalDbError

data UserSession
data UserId
data Email = Email deriving Eq
data ID

validateUserSession :: UserSession -> IO (Maybe UserId)
validateUserSession = error "TODO: Not implemented"

getEmailByUserId :: UserId -> IO (Maybe Email)
getEmailByUserId = error "TODO: Not implemented"

getUserIdByEmail :: Email -> IO (Maybe UserId)
getUserIdByEmail = error "TODO: Not implemented"

insertUserEmail :: UserId -> Email -> IO (Either InternalDbError ID)
insertUserEmail = error "TODO: Not implemented"

withUserSession
    :: UserSession
    -> (UserId -> IO (Either AppError a))
    -> IO (Either AppError a)
withUserSession userSession next = validateUserSession userSession >>= \case
    Nothing -> pure $ Left UserSessionIsInvalid
    Just userId -> next userId

withCheckedUserEmail
    :: UserId
    -> Email
    -> IO (Either AppError a)
    -> IO (Either AppError a)
withCheckedUserEmail userId email next = getEmailByUserId userId >>= \case
    Just otherEmail
        | email == otherEmail -> pure $ Left UserAlreadyHasEmail
        | otherwise -> pure $ Left UserHasDifferentEmail
    Nothing -> next

withCheckedOtherUserEmail
    :: Email
    -> IO (Either AppError a)
    -> IO (Either AppError a)
withCheckedOtherUserEmail email next = getUserIdByEmail email >>= \case
    Just otherUserId -> pure $ Left EmailIsTaken
    Nothing -> next

withEmailInsert
    :: UserId
    -> Email
    -> (ID -> IO (Either AppError a))
    -> IO (Either AppError a)
withEmailInsert userId email next = insertUserEmail userId email >>= \case
    Left dbErr -> pure $ Left $ DbError dbErr
    Right id' -> next id'

associateEmail
    :: UserSession
    -> Email
    -> IO (Either AppError ID)
associateEmail userSession email =
    withUserSession userSession $ \userId ->
    withCheckedUserEmail userId email $
    withCheckedOtherUserEmail email $
    withEmailInsert userId email $ \id' ->
    pure $ Right id'