{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Auth
  ( WikiMusicUser (..),
    LoginRequest (..),
    UserRole (..),
    isAtLeastDemo,
    isAtLeastMaintainer,
    isAtLeastSuperUser,
    isAtLeastLowRank,
    doWithRoles,
    doWithRoles',
    Prelude.show,
    Prelude.read,
  )
where

import Data.Aeson
import Data.OpenApi
import Data.Text qualified as T
import Data.UUID qualified
import Optics
import Relude
import Text.Read
import Prelude qualified

data UserRole = SuperUser | Maintainer | LowRank | Demo
  deriving (Eq, Generic, ToSchema)

instance FromJSON UserRole where
  parseJSON (Data.Aeson.String v) = pure . read . T.unpack $ v
  parseJSON _ = mzero

instance ToJSON UserRole where
  toJSON SuperUser = "wm::superuser"
  toJSON Maintainer = "wm::maintainer"
  toJSON LowRank = "wm::lowrank"
  toJSON Demo = "wm::demo"

instance Show UserRole where
  show SuperUser = "wm::superuser"
  show Maintainer = "wm::maintainer"
  show LowRank = "wm::lowrank"
  show Demo = "wm::demo"

instance Read UserRole where
  readsPrec _ "wm::superuser" = [(SuperUser, "")]
  readsPrec _ "wm::maintainer" = [(Maintainer, "")]
  readsPrec _ "wm::lowrank" = [(LowRank, "")]
  readsPrec _ "wm::demo" = [(Demo, "")]
  readsPrec _ _ = []

isAtLeastDemo :: [UserRole] -> Bool
isAtLeastDemo xs = elem Demo xs || elem LowRank xs || elem Maintainer xs || elem SuperUser xs

isAtLeastLowRank :: [UserRole] -> Bool
isAtLeastLowRank xs = elem LowRank xs || elem Maintainer xs || elem SuperUser xs

isAtLeastMaintainer :: [UserRole] -> Bool
isAtLeastMaintainer xs = elem Maintainer xs || elem SuperUser xs

isAtLeastSuperUser :: [UserRole] -> Bool
isAtLeastSuperUser = elem SuperUser

doWithRoles :: (Applicative f) => t -> (t -> Bool) -> p -> f (Either p b) -> f (Either p b)
doWithRoles roles computeExpectation err eff = if computeExpectation roles then eff else pure . Left $ err

doWithRoles' :: (Applicative f) => WikiMusicUser -> ([UserRole] -> Bool) -> p -> f (Either p b) -> f (Either p b)
doWithRoles' authToken computeExpectation err eff = if computeExpectation (authToken ^. #roles) then eff else pure . Left $ err

data WikiMusicUser = WikiMusicUser
  { identifier :: Data.UUID.UUID,
    displayName :: Text,
    emailAddress :: Text,
    passwordHash :: Maybe Text,
    authToken :: Maybe Text,
    roles :: [UserRole]
  }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

makeFieldLabelsNoPrefix ''WikiMusicUser

data LoginRequest = LoginRequest
  { wikimusicEmail :: String,
    wikimusicPassword :: String
  }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''LoginRequest
