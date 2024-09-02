{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Other
  ( User (..),
    SystemInformationResponse (..),
    Offset (..),
    Limit (..),
    FetchLimit (..),
    SearchInput (..),
  )
where

import Data.Aeson
import Data.OpenApi
import Relude
import Data.Time
import Optics

newtype Limit = Limit Int

newtype Offset = Offset Int

data SystemInformationResponse = SystemInformationResponse
  { reportedVersion :: Text,
    processStartedAt :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''SystemInformationResponse

data User = User
  { identifier :: Text,
    displayName :: Text,
    emailAddress :: Text,
    passwordHash :: Text,
    passwordResetToken :: Maybe Text,
    latestLoginAt :: Maybe UTCTime,
    latestLoginDevice :: Maybe Text,
    avatarUrl :: Maybe Text,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeFieldLabelsNoPrefix ''User

newtype FetchLimit = FetchLimit
  { value :: Int
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''FetchLimit

newtype SearchInput = SearchInput
  { value :: Text
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''SearchInput

-- data ForumPage = ForumPage
--   { identifier :: Text,
--     displayName :: Text,
--     createdBy :: Text,
--     visibilityStatus :: Int,
--     approvedBy :: Maybe Text,
--     createdAt :: Text,
--     lastEditedAt :: Text,
-- viewCount :: Int
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumCategory = ForumCategory
--   { identifier :: Text,
--     parentIdentifier :: Maybe Text,
--     displayName :: Text,
--     createdBy :: Text,
--     visibilityStatus :: Int,
--     approvedBy :: Maybe Text,
--     createdAt :: Text,
--     lastEditedAt :: Text,
-- viewCount :: Int
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data Genre = Genre
--   { identifier :: Text,
--     parentIdentifier :: Maybe Text,
--     displayName :: Text,
--     createdBy :: Text,
--     visibilityStatus :: Int,
--     approvedBy :: Maybe Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserFavouriteSongFolder = UserFavouriteSongFolder
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserFavouriteArtist = UserFavouriteArtist
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumPageComment = ForumPageComment
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumPageContent = ForumPageContent
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumPageExternalSource = ForumPageExternalSource
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumPageOpinion = ForumPageOpinion
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserFavouriteForumPage = UserFavouriteForumPage
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumCategoryArtwork = ForumCategoryArtwork
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumCategoryComment = ForumCategoryComment
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumCategoryExtenalSource = ForumCategoryExtenalSource
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumCategoryOpinion = ForumCategoryOpinion
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserFavouriteForumCategory = UserFavouriteForumCategory
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserFavouriteGenre = UserFavouriteGenre
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserFavouriteSong = UserFavouriteSong
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserDonation = UserDonation
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserRole = UserRole
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data UserTrustScore = UserTrustScore
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data SongToGenres = SongToGenres
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data SongToArtists = SongToArtists
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ArtistToGenres = ArtistToGenres
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- data ForumPageToForumCategories = ForumPageToForumCategories
--   { identifier :: Text,
--     displayName :: Text
--   }
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)
