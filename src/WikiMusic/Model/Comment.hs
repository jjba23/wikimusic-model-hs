{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Comment
  ( Comment (..),
    parseCommentRows,
  )
where

import Data.Aeson hiding (Success)
import Data.OpenApi
import Data.Time
import Data.UUID
import Relude
import Optics

data Comment = Comment
  { identifier :: UUID,
    parentIdentifier :: Maybe UUID,
    createdBy :: UUID,
    visibilityStatus :: Int,
    contents :: Text,
    approvedBy :: Maybe UUID,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''Comment

parseCommentRows :: (JoinKinds k1 l k2, Is k2 A_Getter, LabelOptic "identifier" l u v a1 a1, LabelOptic "comment" k1 b b u v) => (a2 -> b) -> [a2] -> [(a1, b)]
parseCommentRows fromRow = map ((\x -> (x ^. #comment % #identifier, x)) . fromRow)
