{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.Interaction.Model.Song
  ( Song (..),
    SongArtworkOrderUpdateRequest (..),
    SongArtwork (..),
    SongComment (..),
    SongOpinion (..),
    GetSongsQueryResponse (..),
    InsertSongsCommandResponse (..),
    InsertSongsRequest (..),
    InsertSongsRequestItem (..),
    InsertSongCommentsCommandResponse (..),
    InsertSongCommentsRequest (..),
    InsertSongCommentsRequestItem (..),
    UpsertSongOpinionsCommandResponse (..),
    UpsertSongOpinionsRequest (..),
    UpsertSongOpinionsRequestItem (..),
    InsertSongArtworksCommandResponse (..),
    InsertSongArtworksRequest (..),
    InsertSongArtworksRequestItem (..),
    parseInclude,
    InsertArtistsOfSongsRequest (..),
    InsertArtistsOfSongsRequestItem (..),
    InsertArtistsOfSongCommandResponse (..),
    SongDeltaRequest (..),
    ifAllValid,
    SongError (..),
    SongContentDeltaRequest (..),
    InsertSongContentsRequestItem (..),
    InsertSongContentsRequest (..),
    InsertSongContentsCommandResponse (..),
  )
where

import Data.Aeson hiding (Success)
import Data.OpenApi
import Data.UUID hiding (null)
import Keuringsdienst
import Keuringsdienst.Helpers
import Optics
import Relude
import WikiMusic.Model.Song

instance ToSchema (Validation [Text])

data GetSongsQueryResponse = GetSongsQueryResponse
  { songs :: Map UUID Song,
    sortOrder :: [UUID]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''GetSongsQueryResponse

data InsertSongsCommandResponse = InsertSongsQueryResponse
  { songs :: Map UUID Song,
    sortOrder :: [UUID],
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongsCommandResponse

data InsertSongsRequestItem = InsertSongsRequestItem
  { displayName :: Text,
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

newtype InsertSongsRequest = InsertSongsRequest
  { songs :: [InsertSongsRequestItem]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongsRequest
makeFieldLabelsNoPrefix ''InsertSongsRequestItem

-- song comments

data InsertSongCommentsCommandResponse = InsertSongCommentsCommandResponse
  { songComments :: Map UUID SongComment,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongCommentsCommandResponse

data InsertSongCommentsRequestItem = InsertSongCommentsRequestItem
  { songIdentifier :: UUID,
    parentIdentifier :: Maybe UUID,
    contents :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertSongCommentsRequest = InsertSongCommentsRequest
  { songComments :: [InsertSongCommentsRequestItem]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongCommentsRequest
makeFieldLabelsNoPrefix ''InsertSongCommentsRequestItem

-- song opinions
data UpsertSongOpinionsCommandResponse = UpsertSongOpinionsCommandResponse
  { songOpinions :: Map UUID SongOpinion,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''UpsertSongOpinionsCommandResponse

data UpsertSongOpinionsRequestItem = UpsertSongOpinionsRequestItem
  { songIdentifier :: UUID,
    isLike :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype UpsertSongOpinionsRequest = UpsertSongOpinionsRequest
  { songOpinions :: [UpsertSongOpinionsRequestItem]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''UpsertSongOpinionsRequest
makeFieldLabelsNoPrefix ''UpsertSongOpinionsRequestItem

-- song artworks

data InsertSongArtworksCommandResponse = InsertSongArtworksCommandResponse
  { songArtworks :: Map UUID SongArtwork,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongArtworksCommandResponse

data InsertArtistsOfSongCommandResponse = InsertArtistsOfSongCommandResponse
  { songArtists :: Map UUID ArtistOfSong,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertArtistsOfSongCommandResponse

data InsertSongArtworksRequestItem = InsertSongArtworksRequestItem
  { songIdentifier :: UUID,
    contentUrl :: Text,
    contentCaption :: Maybe Text,
    orderValue :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertSongArtworksRequest = InsertSongArtworksRequest
  { songArtworks :: [InsertSongArtworksRequestItem]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongArtworksRequest
makeFieldLabelsNoPrefix ''InsertSongArtworksRequestItem

data InsertArtistsOfSongsRequestItem = InsertArtistsOfSongsRequestItem
  { songIdentifier :: UUID,
    artistIdentifier :: UUID
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertArtistsOfSongsRequest = InsertArtistsOfSongsRequest
  { songArtists :: [InsertArtistsOfSongsRequestItem]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertArtistsOfSongsRequest
makeFieldLabelsNoPrefix ''InsertArtistsOfSongsRequestItem

newtype SongArtworkOrderUpdateRequest = SongArtworkOrderUpdateRequest
  { songArtworkOrders :: [SongArtworkOrderUpdate]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''SongArtworkOrderUpdateRequest

newtype SongDeltaRequest = SongDeltaRequest
  { songDeltas :: [SongDelta]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''SongDeltaRequest

newtype SongContentDeltaRequest = SongContentDeltaRequest
  { songContentDeltas :: [SongContentDelta]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''SongContentDeltaRequest

data InsertSongContentsRequestItem = InsertSongContentsRequestItem
  { songIdentifier :: UUID,
    versionName :: Text,
    instrumentType :: Text,
    asciiLegend :: Maybe Text,
    asciiContents :: Maybe Text,
    pdfContents :: Maybe Text,
    guitarProContents :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongContentsRequestItem

newtype InsertSongContentsRequest = InsertSongContentsRequest
  { songContents :: [InsertSongContentsRequestItem]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongContentsRequest

data InsertSongContentsCommandResponse = InsertSongContentsCommandResponse
  { songContents :: Map UUID SongContent,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertSongContentsCommandResponse

data SongError
  = ValidationFailedError (Map Text ValidationResult)
  | AccessUnauthorizedError
  | SomeError Text
  deriving (Show, Eq, Generic)

ifAllValid ::
  (Applicative f) =>
  Map Text (Validation [Text]) ->
  f (Either SongError b) ->
  f (Either SongError b)
ifAllValid validationResults eff = do
  if null $ filterFailedValidations validationResults
    then do eff
    else pure . Left $ ValidationFailedError validationResults
