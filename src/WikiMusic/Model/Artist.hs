{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Artist
  ( Artist (..),
    ArtistArtwork (..),
    ArtistComment (..),
    ArtistOpinion (..),
    validateArtist,
    validateArtistArtwork,
    validateArtistComment,
    validateArtistOpinion,
    ArtistSortOrder (..),
    ArtistExternalSources (..),
    validateArtistExternalSources,
    ArtistArtworkOrderUpdate (..),
    validateArtistArtworkOrderUpdate,
    validateArtistDelta,
    ArtistDelta (..),
    Prelude.show,
    Prelude.read,
    ArtistIncludes (..),
    EnrichArtistParams (..),
    fullEnrichment,
    noEnrichment,
    parseInclude,
  )
where

import Data.Aeson hiding (Success)
import Data.Map qualified as Map
import Data.OpenApi
import Data.Text qualified as T
import Data.Time
import Data.UUID
import Keuringsdienst
import Keuringsdienst.Helpers
import Relude
import Text.Read
import WikiMusic.Model.Artwork
import WikiMusic.Model.Comment
import WikiMusic.Model.Opinion
import WikiMusic.Model.Thread
import Prelude qualified
import Optics

data ArtistIncludes = IncludeComments | IncludeOpinions | IncludeArtworks deriving (Eq, Generic, FromJSON, ToJSON, ToSchema)

instance Show ArtistIncludes where
  show IncludeComments = "comments"
  show IncludeOpinions = "opinions"
  show IncludeArtworks = "artworks"

instance Read ArtistIncludes where
  readsPrec _ "comments" = [(IncludeComments, "")]
  readsPrec _ "opinions" = [(IncludeOpinions, "")]
  readsPrec _ "artworks" = [(IncludeArtworks, "")]
  readsPrec _ _ = []

data ArtistSortOrder
  = DescCreatedAt
  | AscCreatedAt
  | DescLastEditedAt
  | AscLastEditedAt
  | DescDisplayName
  | AscDisplayName

instance Show ArtistSortOrder where
  show DescCreatedAt = "artists.created_at DESC"
  show AscCreatedAt = "artists.created_at ASC"
  show DescLastEditedAt = "artists.last_edited_at DESC"
  show AscLastEditedAt = "artists.last_edited_at ASC"
  show DescDisplayName = "artists.display_name DESC"
  show AscDisplayName = "artists.display_name ASC"

instance Read ArtistSortOrder where
  readsPrec _ "created-at-desc" = [(DescCreatedAt, "")]
  readsPrec _ "created-at-asc" = [(AscCreatedAt, "")]
  readsPrec _ "last-edited-at-desc" = [(DescLastEditedAt, "")]
  readsPrec _ "last-edited-at-asc" = [(AscLastEditedAt, "")]
  readsPrec _ "display-name-desc" = [(DescDisplayName, "")]
  readsPrec _ "display-name-asc" = [(AscDisplayName, "")]
  readsPrec _ _ = []

validateArtistOpinion :: ArtistOpinion -> ValidationResult
validateArtistOpinion x =
  (x ^. #opinion % #isLike)
    |?| isNotEqualTo (x ^. #opinion % #isDislike)
    <> (x ^. #opinion % #isDislike)
      |?| isNotEqualTo (x ^. #opinion % #isLike)

data ArtistOpinion = ArtistOpinion
  { artistIdentifier :: UUID,
    opinion :: Opinion
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateArtistArtwork :: ArtistArtwork -> ValidationResult
validateArtistArtwork x =
  (x ^. #artwork % #contentUrl)
    |?| isNonEmptyText
    <> (x ^. #artwork % #contentCaption)
      |??| isNonEmptyText
    <> (x ^. #artwork % #visibilityStatus)
      |?| isPositiveOrZero
    <> (x ^. #artwork % #orderValue)
      |?| isPositiveOrZero

data ArtistArtwork = ArtistArtwork
  { artistIdentifier :: UUID,
    artwork :: Artwork
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateArtistComment :: ArtistComment -> ValidationResult
validateArtistComment x =
  (x ^. #comment % #visibilityStatus)
    |?| isPositiveOrZero
    <> (x ^. #comment % #contents)
      |?| (isNonEmptyText <> isTextSmallerThanOrEqual 8200)

data ArtistComment = ArtistComment
  { artistIdentifier :: UUID,
    comment :: Comment
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateArtist :: Artist -> ValidationResult
validateArtist x =
  (x ^. #displayName)
    |?| (isNonEmptyText <> isTextSmallerThanOrEqual 340)
    <> (x ^. #visibilityStatus)
      |?| isPositiveOrZero
    <> (x ^. #spotifyUrl)
      |??| isNonEmptyText
    <> (x ^. #youtubeUrl)
      |??| isNonEmptyText
    <> (x ^. #soundcloudUrl)
      |??| isNonEmptyText
    <> (x ^. #wikipediaUrl)
      |??| isNonEmptyText

data Artist = Artist
  { identifier :: UUID,
    displayName :: Text,
    createdBy :: UUID,
    visibilityStatus :: Int,
    approvedBy :: Maybe UUID,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime,
    artworks :: Map UUID ArtistArtwork,
    comments :: [ThreadRender ArtistComment],
    opinions :: Map UUID ArtistOpinion,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    viewCount :: Int,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''Artist
makeFieldLabelsNoPrefix ''ArtistComment
makeFieldLabelsNoPrefix ''ArtistArtwork
makeFieldLabelsNoPrefix ''ArtistOpinion

validateArtistExternalSources :: ArtistExternalSources -> ValidationResult
validateArtistExternalSources x =
  (x ^. #spotifyUrl)
    |??| isNonEmptyText
    <> (x ^. #youtubeUrl)
      |??| isNonEmptyText
    <> (x ^. #soundcloudUrl)
      |??| isNonEmptyText
    <> (x ^. #wikipediaUrl)
      |??| isNonEmptyText

data ArtistExternalSources = ArtistExternalSources
  { identifier :: UUID,
    artistIdentifier :: UUID,
    createdBy :: UUID,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''ArtistExternalSources

validateArtistArtworkOrderUpdate :: ArtistArtworkOrderUpdate -> ValidationResult
validateArtistArtworkOrderUpdate x =
  (x ^. #orderValue) |?| isPositiveOrZero

data ArtistArtworkOrderUpdate = ArtistArtworkOrderUpdate
  { identifier :: UUID,
    orderValue :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data ArtistDelta = ArtistDelta
  { identifier :: UUID,
    displayName :: Maybe Text,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''ArtistDelta

validateArtistDelta :: ArtistDelta -> ValidationResult
validateArtistDelta x =
  (x ^. #displayName)
    |??| (isNonEmptyText <> isTextSmallerThanOrEqual 340)
    <> (x ^. #spotifyUrl)
      |??| isNonEmptyText
    <> (x ^. #youtubeUrl)
      |??| isNonEmptyText
    <> (x ^. #soundcloudUrl)
      |??| isNonEmptyText
    <> (x ^. #wikipediaUrl)
      |??| isNonEmptyText

data EnrichArtistParams = EnrichArtistParams
  { includeComments :: Bool,
    includeOpinions :: Bool,
    includeArtworks :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''EnrichArtistParams

fullEnrichment :: EnrichArtistParams
fullEnrichment =
  EnrichArtistParams
    { includeComments = True,
      includeArtworks = True,
      includeOpinions = True
    }

noEnrichment :: EnrichArtistParams
noEnrichment =
  EnrichArtistParams
    { includeComments = False,
      includeArtworks = False,
      includeOpinions = False
    }

parseInclude :: Text -> EnrichArtistParams
parseInclude includeString = do
  EnrichArtistParams
    { includeComments = fromIncludeMap IncludeComments,
      includeOpinions = fromIncludeMap IncludeOpinions,
      includeArtworks = fromIncludeMap IncludeArtworks
    }
  where
    includes = Map.fromList $ map (,True) (T.split (== ',') includeString)
    fromIncludeMap k = fromMaybe False (includes Map.!? (T.pack . show $ k))
