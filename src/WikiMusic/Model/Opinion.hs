{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Opinion
  ( Opinion (..),
    parseOpinionRows,
  )
where

import Data.Aeson hiding (Success)
import Data.OpenApi
import Data.Time
import Data.UUID
import Relude
import Optics

data Opinion = Opinion
  { identifier :: UUID,
    createdBy :: UUID,
    isLike :: Bool,
    isDislike :: Bool,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''Opinion

parseOpinionRows :: (JoinKinds k1 l k2, Is k2 A_Getter, LabelOptic "identifier" l u v a1 a1, LabelOptic "opinion" k1 b b u v) => (a2 -> b) -> [a2] -> [(a1, b)]
parseOpinionRows fromRow = map ((\x -> (x ^. #opinion % #identifier, x)) . fromRow)
