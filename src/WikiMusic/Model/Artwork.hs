{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Artwork
  ( Artwork (..),
    parseArtworkRows,
  )
where

import Data.Aeson
import Data.OpenApi
import Relude
import Data.Time
import Data.UUID
import Optics

data Artwork = Artwork
  { identifier :: UUID,
    createdBy :: UUID,
    visibilityStatus :: Int,
    approvedBy :: Maybe UUID,
    contentUrl :: Text,
    contentCaption :: Maybe Text,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime,
    orderValue :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''Artwork

parseArtworkRows :: (JoinKinds k1 l k2, Is k2 A_Getter, LabelOptic "identifier" l u v a1 a1, LabelOptic "artwork" k1 b b u v) => (a2 -> b) -> [a2] -> [(a1, b)]
parseArtworkRows fromRow = map ((\x -> (x ^. #artwork % #identifier, x)) . fromRow)
