{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Mail
  ( MailCommandError (..),
    MailCommandOutcome (..),
    MailSendRequest (..),
  )
where

import Data.Aeson hiding (Success)
import Optics
import Relude

data MailCommandError = MailError Text | LogicError Text | NetworkError Text deriving (Show)

data MailCommandOutcome = MailSent | MailNotSent

data MailSendRequest = MailSendRequest
  { subject :: Text,
    email :: Text,
    name :: Maybe Text,
    body :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

makeFieldLabelsNoPrefix ''MailSendRequest
