{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Interaction.Model.Auth
  ( GetMeQueryResponse (..),
  )
where

import Data.Aeson
import Data.OpenApi
import Data.UUID
import Optics
import Relude
import WikiMusic.Model.Auth

data GetMeQueryResponse = GetMeQueryResponse
  { identifier :: UUID,
    displayName :: Text,
    emailAddress :: Text,
    roles :: [UserRole]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''GetMeQueryResponse
