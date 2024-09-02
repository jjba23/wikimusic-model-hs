{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Song
  ( Song (..),
    SongArtwork (..),
    SongComment (..),
    SongOpinion (..),
    SongContent (..),
    validateSong,
    validateSongArtwork,
    validateSongComment,
    validateSongOpinion,
    validateSongContent,
    SongSortOrder (..),
    validateArtistOfSong,
    ArtistOfSong (..),
    SongExternalSources (..),
    validateSongExternalSources,
    SongArtworkOrderUpdate (..),
    validateSongArtworkOrderUpdate,
    validateSongDelta,
    SongDelta (..),
    validateSongContentDelta,
    SongContentDelta (..),
    Prelude.show,
    Prelude.read,
    SongIncludes (..),
    EnrichSongParams (..),
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

data SongSortOrder
  = DescCreatedAt
  | AscCreatedAt
  | DescLastEditedAt
  | AscLastEditedAt
  | DescDisplayName
  | AscDisplayName

instance Show SongSortOrder where
  show DescCreatedAt = "songs.created_at DESC"
  show AscCreatedAt = "songs.created_at ASC"
  show DescLastEditedAt = "songs.last_edited_at DESC"
  show AscLastEditedAt = "songs.last_edited_at ASC"
  show DescDisplayName = "songs.display_name DESC"
  show AscDisplayName = "songs.display_name ASC"

instance Read SongSortOrder where
  readsPrec _ "created-at-desc" = [(DescCreatedAt, "")]
  readsPrec _ "created-at-asc" = [(AscCreatedAt, "")]
  readsPrec _ "last-edited-at-desc" = [(DescLastEditedAt, "")]
  readsPrec _ "last-edited-at-asc" = [(AscLastEditedAt, "")]
  readsPrec _ "display-name-desc" = [(DescDisplayName, "")]
  readsPrec _ "display-name-asc" = [(AscDisplayName, "")]
  readsPrec _ _ = []

data SongContent = SongContent
  { identifier :: UUID,
    songIdentifier :: UUID,
    versionName :: Text,
    createdBy :: UUID,
    visibilityStatus :: Int,
    approvedBy :: Maybe UUID,
    instrumentType :: Text,
    asciiLegend :: Maybe Text,
    asciiContents :: Maybe Text,
    pdfContents :: Maybe Text,
    guitarProContents :: Maybe Text,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''SongContent

data SongOpinion = SongOpinion
  { songIdentifier :: UUID,
    opinion :: Opinion
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateSongArtwork :: SongArtwork -> ValidationResult
validateSongArtwork x =
  (x ^. #artwork % #contentUrl)
    |?| isNonEmptyText
    <> (x ^. #artwork % #contentCaption)
      |??| isNonEmptyText
    <> (x ^. #artwork % #visibilityStatus)
      |?| isPositiveOrZero
    <> (x ^. #artwork % #orderValue)
      |?| isPositiveOrZero

data SongArtwork = SongArtwork
  { songIdentifier :: UUID,
    artwork :: Artwork
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateSongComment :: SongComment -> ValidationResult
validateSongComment x =
  (x ^. #comment % #visibilityStatus)
    |?| isPositiveOrZero
    <> (x ^. #comment % #contents)
      |?| (isNonEmptyText <> isTextSmallerThanOrEqual 8200)

data SongComment = SongComment
  { songIdentifier :: UUID,
    comment :: Comment
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateSongExternalSources :: SongExternalSources -> ValidationResult
validateSongExternalSources x =
  (x ^. #spotifyUrl)
    |??| isNonEmptyText
    <> (x ^. #youtubeUrl)
      |??| isNonEmptyText
    <> (x ^. #soundcloudUrl)
      |??| isNonEmptyText
    <> (x ^. #wikipediaUrl)
      |??| isNonEmptyText

data SongExternalSources = SongExternalSources
  { identifier :: UUID,
    songIdentifier :: UUID,
    createdBy :: UUID,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''SongExternalSources

data Song = Song
  { identifier :: UUID,
    displayName :: Text,
    musicKey :: Maybe Text,
    musicTuning :: Maybe Text,
    musicCreationDate :: Maybe Text,
    albumName :: Maybe Text,
    albumInfoLink :: Maybe Text,
    createdBy :: UUID,
    visibilityStatus :: Int,
    approvedBy :: Maybe UUID,
    createdAt :: UTCTime,
    lastEditedAt :: Maybe UTCTime,
    artworks :: Map UUID SongArtwork,
    comments :: [ThreadRender SongComment],
    opinions :: Map UUID SongOpinion,
    contents :: Map UUID SongContent,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    artists :: Map UUID Text,
    viewCount :: Int,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data SongArtworkOrderUpdate = SongArtworkOrderUpdate
  { identifier :: UUID,
    orderValue :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data EnrichSongParams = EnrichSongParams
  { includeComments :: Bool,
    includeOpinions :: Bool,
    includeArtworks :: Bool,
    includeArtists :: Bool,
    includeContents :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''EnrichSongParams

data SongIncludes
  = IncludeArtists
  | IncludeComments
  | IncludeOpinions
  | IncludeArtworks
  | IncludeContents
  deriving (Eq, Generic, FromJSON, ToJSON, ToSchema)

instance Show SongIncludes where
  show IncludeComments = "comments"
  show IncludeOpinions = "opinions"
  show IncludeArtworks = "artworks"
  show IncludeArtists = "artists"
  show IncludeContents = "contents"

instance Read SongIncludes where
  readsPrec _ "comments" = [(IncludeComments, "")]
  readsPrec _ "opinions" = [(IncludeOpinions, "")]
  readsPrec _ "artworks" = [(IncludeArtworks, "")]
  readsPrec _ "artists" = [(IncludeArtists, "")]
  readsPrec _ "contents" = [(IncludeContents, "")]
  readsPrec _ _ = []

fullEnrichment :: EnrichSongParams
fullEnrichment =
  EnrichSongParams
    { includeComments = True,
      includeArtworks = True,
      includeOpinions = True,
      includeArtists = True,
      includeContents = True
    }

noEnrichment :: EnrichSongParams
noEnrichment =
  EnrichSongParams
    { includeComments = False,
      includeArtworks = False,
      includeOpinions = False,
      includeArtists = False,
      includeContents = False
    }

parseInclude :: Text -> EnrichSongParams
parseInclude includeString = do
  EnrichSongParams
    { includeComments = fromIncludeMap IncludeComments,
      includeOpinions = fromIncludeMap IncludeOpinions,
      includeArtworks = fromIncludeMap IncludeArtworks,
      includeArtists = fromIncludeMap IncludeArtists,
      includeContents = fromIncludeMap IncludeContents
    }
  where
    includes = Map.fromList $ map (,True) (T.split (== ',') includeString)
    fromIncludeMap k = fromMaybe False (includes Map.!? (T.pack . show $ k))

validateArtistOfSong :: ArtistOfSong -> ValidationResult
validateArtistOfSong _ = Success

data ArtistOfSong = ArtistOfSong
  { identifier :: UUID,
    songIdentifier :: UUID,
    artistIdentifier :: UUID,
    createdAt :: UTCTime,
    createdBy :: UUID
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

validateSongContent :: SongContent -> ValidationResult
validateSongContent x =
  (x ^. #versionName)
    |?| isTextSmallerThanOrEqual 400
    <> (x ^. #instrumentType)
      |?| isNonEmptyText
    <> (x ^. #asciiLegend)
      |??| isNonEmptyText
    <> (x ^. #asciiContents)
      |??| isNonEmptyText

validateSongContentDelta :: SongContentDelta -> ValidationResult
validateSongContentDelta x =
  (x ^. #versionName)
    |?| isTextSmallerThanOrEqual 400
    <> (x ^. #instrumentType)
      |??| isNonEmptyText
    <> (x ^. #asciiLegend)
      |??| isNonEmptyText
    <> (x ^. #asciiContents)
      |??| isNonEmptyText

data SongContentDelta = SongContentDelta
  { identifier :: UUID,
    versionName :: Text,
    instrumentType :: Maybe Text,
    asciiLegend :: Maybe Text,
    asciiContents :: Maybe Text,
    pdfContents :: Maybe Text,
    guitarProContents :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''Song
makeFieldLabelsNoPrefix ''SongComment
makeFieldLabelsNoPrefix ''SongArtwork
makeFieldLabelsNoPrefix ''SongOpinion

makeFieldLabelsNoPrefix ''ArtistOfSong

data SongDelta = SongDelta
  { identifier :: UUID,
    displayName :: Maybe Text,
    musicKey :: Maybe Text,
    musicTuning :: Maybe Text,
    musicCreationDate :: Maybe Text,
    albumName :: Maybe Text,
    albumInfoLink :: Maybe Text,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''SongDelta

validateSongDelta :: SongDelta -> ValidationResult
validateSongDelta x =
  (x ^. #displayName)
    |??| (isNonEmptyText <> isTextSmallerThanOrEqual 340)
    <> (x ^. #musicKey)
      |??| (isNonEmptyText <> isTextSmallerThanOrEqual 100)
    <> (x ^. #musicTuning)
      |??| (isNonEmptyText <> isTextSmallerThanOrEqual 100)
    <> (x ^. #musicCreationDate)
      |??| (isNonEmptyText <> isTextSmallerThanOrEqual 340)
    <> (x ^. #albumName)
      |??| (isNonEmptyText <> isTextSmallerThanOrEqual 340)
    <> (x ^. #albumInfoLink)
      |??| isNonEmptyText
    <> (x ^. #spotifyUrl)
      |??| isNonEmptyText
    <> (x ^. #youtubeUrl)
      |??| isNonEmptyText
    <> (x ^. #soundcloudUrl)
      |??| isNonEmptyText
    <> (x ^. #wikipediaUrl)
      |??| isNonEmptyText

validateSong :: Song -> ValidationResult
validateSong x =
  (x ^. #displayName)
    |?| (isNonEmptyText <> isTextSmallerThanOrEqual 340)
    <> (x ^. #musicKey)
      |??| (isNonEmptyText <> isTextSmallerThanOrEqual 50)
    <> (x ^. #musicTuning)
      |??| (isNonEmptyText <> isTextSmallerThanOrEqual 50)
    <> (x ^. #musicCreationDate)
      |??| (isNonEmptyText <> isTextSmallerThanOrEqual 340)
    <> (x ^. #albumName)
      |??| (isNonEmptyText <> isTextSmallerThanOrEqual 340)
    <> (x ^. #albumInfoLink)
      |??| isNonEmptyText
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

validateSongOpinion :: SongOpinion -> ValidationResult
validateSongOpinion x =
  (x ^. #opinion % #isLike)
    |?| isNotEqualTo (x ^. #opinion % #isDislike)
    <> (x ^. #opinion % #isDislike)
      |?| isNotEqualTo (x ^. #opinion % #isLike)

validateSongArtworkOrderUpdate :: SongArtworkOrderUpdate -> ValidationResult
validateSongArtworkOrderUpdate x =
  (x ^. #orderValue) |?| isPositiveOrZero
