{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Genre
  ( Genre (..),
    GenreArtwork (..),
    GenreComment (..),
    GenreOpinion (..),
    validateGenre,
    validateGenreArtwork,
    validateGenreComment,
    validateGenreOpinion,
    GenreSortOrder (..),
    GenreExternalSources (..),
    validateGenreExternalSources,
    GenreArtworkOrderUpdate (..),
    validateGenreArtworkOrderUpdate,
    validateGenreDelta,
    GenreDelta (..),
    Prelude.show,
    Prelude.read,
    GenreIncludes (..),
    EnrichGenreParams (..),
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
import Optics
import Relude
import Text.Read
import WikiMusic.Model.Artwork
import WikiMusic.Model.Comment
import WikiMusic.Model.Opinion
import WikiMusic.Model.Thread
import Prelude qualified

data GenreIncludes = IncludeComments | IncludeOpinions | IncludeArtworks deriving (Eq, Generic, FromJSON, ToJSON, ToSchema)

instance Show GenreIncludes where
  show IncludeComments = "comments"
  show IncludeOpinions = "opinions"
  show IncludeArtworks = "artworks"

instance Read GenreIncludes where
  readsPrec _ "comments" = [(IncludeComments, "")]
  readsPrec _ "opinions" = [(IncludeOpinions, "")]
  readsPrec _ "artworks" = [(IncludeArtworks, "")]
  readsPrec _ _ = []

data GenreSortOrder
  = DescCreatedAt
  | AscCreatedAt
  | DescLastEditedAt
  | AscLastEditedAt
  | DescDisplayName
  | AscDisplayName

instance Show GenreSortOrder where
  show DescCreatedAt = "genres.created_at DESC"
  show AscCreatedAt = "genres.created_at ASC"
  show DescLastEditedAt = "genres.last_edited_at DESC"
  show AscLastEditedAt = "genres.last_edited_at ASC"
  show DescDisplayName = "genres.display_name DESC"
  show AscDisplayName = "genres.display_name ASC"

instance Read GenreSortOrder where
  readsPrec _ "created-at-desc" = [(DescCreatedAt, "")]
  readsPrec _ "created-at-asc" = [(AscCreatedAt, "")]
  readsPrec _ "last-edited-at-desc" = [(DescLastEditedAt, "")]
  readsPrec _ "last-edited-at-asc" = [(AscLastEditedAt, "")]
  readsPrec _ "display-name-desc" = [(DescDisplayName, "")]
  readsPrec _ "display-name-asc" = [(AscDisplayName, "")]
  readsPrec _ _ = []

validateGenreOpinion :: GenreOpinion -> ValidationResult
validateGenreOpinion x =
  (x ^. #opinion % #isLike)
    |?| isNotEqualTo (x ^. #opinion % #isDislike)
    <> (x ^. #opinion % #isDislike)
      |?| isNotEqualTo (x ^. #opinion % #isLike)

data GenreOpinion = GenreOpinion
  { genreIdentifier :: UUID,
    opinion :: Opinion
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateGenreArtwork :: GenreArtwork -> ValidationResult
validateGenreArtwork x =
  (x ^. #artwork % #contentUrl)
    |?| isNonEmptyText
    <> (x ^. #artwork % #contentCaption)
      |??| isNonEmptyText
    <> (x ^. #artwork % #visibilityStatus)
      |?| isPositiveOrZero
    <> (x ^. #artwork % #orderValue)
      |?| isPositiveOrZero

data GenreArtwork = GenreArtwork
  { genreIdentifier :: UUID,
    artwork :: Artwork
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateGenreComment :: GenreComment -> ValidationResult
validateGenreComment x =
  (x ^. #comment % #visibilityStatus)
    |?| isPositiveOrZero
    <> (x ^. #comment % #contents)
      |?| (isNonEmptyText <> isTextSmallerThanOrEqual 8200)

data GenreComment = GenreComment
  { genreIdentifier :: UUID,
    comment :: Comment
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateGenre :: Genre -> ValidationResult
validateGenre x =
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

data Genre = Genre
  { identifier :: UUID,
    parentIdentifier :: Maybe UUID,
    displayName :: Text,
    createdBy :: UUID,
    visibilityStatus :: Int,
    approvedBy :: Maybe UUID,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime,
    artworks :: Map UUID GenreArtwork,
    comments :: [ThreadRender GenreComment],
    opinions :: Map UUID GenreOpinion,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    viewCount :: Int,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''Genre
makeFieldLabelsNoPrefix ''GenreComment
makeFieldLabelsNoPrefix ''GenreArtwork
makeFieldLabelsNoPrefix ''GenreOpinion

validateGenreExternalSources :: GenreExternalSources -> ValidationResult
validateGenreExternalSources x =
  (x ^. #spotifyUrl)
    |??| isNonEmptyText
    <> (x ^. #youtubeUrl)
      |??| isNonEmptyText
    <> (x ^. #soundcloudUrl)
      |??| isNonEmptyText
    <> (x ^. #wikipediaUrl)
      |??| isNonEmptyText

data GenreExternalSources = GenreExternalSources
  { identifier :: UUID,
    genreIdentifier :: UUID,
    createdBy :: UUID,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''GenreExternalSources

validateGenreArtworkOrderUpdate :: GenreArtworkOrderUpdate -> ValidationResult
validateGenreArtworkOrderUpdate x =
  (x ^. #orderValue) |?| isPositiveOrZero

data GenreArtworkOrderUpdate = GenreArtworkOrderUpdate
  { identifier :: UUID,
    orderValue :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data GenreDelta = GenreDelta
  { identifier :: UUID,
    displayName :: Maybe Text,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''GenreDelta

validateGenreDelta :: GenreDelta -> ValidationResult
validateGenreDelta x =
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

data EnrichGenreParams = EnrichGenreParams
  { includeComments :: Bool,
    includeOpinions :: Bool,
    includeArtworks :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''EnrichGenreParams

fullEnrichment :: EnrichGenreParams
fullEnrichment =
  EnrichGenreParams
    { includeComments = True,
      includeArtworks = True,
      includeOpinions = True
    }

noEnrichment :: EnrichGenreParams
noEnrichment =
  EnrichGenreParams
    { includeComments = False,
      includeArtworks = False,
      includeOpinions = False
    }

parseInclude :: Text -> EnrichGenreParams
parseInclude includeString = do
  EnrichGenreParams
    { includeComments = fromIncludeMap IncludeComments,
      includeOpinions = fromIncludeMap IncludeOpinions,
      includeArtworks = fromIncludeMap IncludeArtworks
    }
  where
    includes = Map.fromList $ map (,True) (T.split (== ',') includeString)
    fromIncludeMap k = fromMaybe False (includes Map.!? (T.pack . show $ k))
