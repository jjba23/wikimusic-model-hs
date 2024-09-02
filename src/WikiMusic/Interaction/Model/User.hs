{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Interaction.Model.User
  ( MakeResetPasswordLinkResponse (..),
    DoPasswordResetRequest (..),
    UserError (..),
    InviteUsersRequest (..),
    DeleteUsersRequest (..),
  )
where

import Data.Aeson hiding (Success)
import Data.OpenApi
import Keuringsdienst (ValidationResult)
import Optics
import Relude
import WikiMusic.Model.Auth

newtype MakeResetPasswordLinkResponse = MakeResetPasswordLinkResponse
  { user :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''MakeResetPasswordLinkResponse

data DoPasswordResetRequest = DoPasswordResetRequest
  { email :: Text,
    token :: Text,
    password :: Text,
    passwordConfirm :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''DoPasswordResetRequest

data UserError
  = ValidationFailedError (Map Text ValidationResult)
  | AccessUnauthorizedError
  | SomeError Text
  deriving (Generic, Eq, Show)

data InviteUsersRequest = InviteUsersRequest
  { displayName :: Text,
    email :: Text,
    role :: UserRole,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InviteUsersRequest

newtype DeleteUsersRequest = DeleteUsersRequest
  { email :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''DeleteUsersRequest
