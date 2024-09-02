{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Interaction.Model.Artist
  ( ArtistError (..),
    ifAllValid,
    Artist (..),
    ArtistArtwork (..),
    ArtistComment (..),
    ArtistOpinion (..),
    GetArtistsQueryResponse (..),
    InsertArtistsCommandResponse (..),
    InsertArtistsRequest (..),
    InsertArtistsRequestItem (..),
    InsertArtistCommentsCommandResponse (..),
    InsertArtistCommentsRequest (..),
    InsertArtistCommentsRequestItem (..),
    UpsertArtistOpinionsCommandResponse (..),
    UpsertArtistOpinionsRequest (..),
    UpsertArtistOpinionsRequestItem (..),
    InsertArtistArtworksCommandResponse (..),
    InsertArtistArtworksRequest (..),
    InsertArtistArtworksRequestItem (..),
    ArtistArtworkOrderUpdateRequest (..),
    ArtistDeltaRequest (..),
  )
where

import Data.Aeson hiding (Success)
import Data.OpenApi
import Data.UUID hiding (null)
import Keuringsdienst
import Keuringsdienst.Helpers
import Optics
import Relude
import WikiMusic.Model.Artist

instance ToSchema (Validation [Text])

data GetArtistsQueryResponse = GetArtistsQueryResponse
  { artists :: Map UUID Artist,
    sortOrder :: [UUID]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''GetArtistsQueryResponse

data InsertArtistsCommandResponse = InsertArtistsQueryResponse
  { artists :: Map UUID Artist,
    sortOrder :: [UUID],
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertArtistsCommandResponse

data InsertArtistsRequestItem = InsertArtistsRequestItem
  { displayName :: Text,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertArtistsRequest = InsertArtistsRequest
  { artists :: [InsertArtistsRequestItem]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertArtistsRequest
makeFieldLabelsNoPrefix ''InsertArtistsRequestItem

-- artist comments

data InsertArtistCommentsCommandResponse = InsertArtistCommentsCommandResponse
  { artistComments :: Map UUID ArtistComment,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertArtistCommentsCommandResponse

data InsertArtistCommentsRequestItem = InsertArtistCommentsRequestItem
  { artistIdentifier :: UUID,
    parentIdentifier :: Maybe UUID,
    contents :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertArtistCommentsRequest = InsertArtistCommentsRequest
  { artistComments :: [InsertArtistCommentsRequestItem]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertArtistCommentsRequest
makeFieldLabelsNoPrefix ''InsertArtistCommentsRequestItem

-- artist opinions
data UpsertArtistOpinionsCommandResponse = UpsertArtistOpinionsCommandResponse
  { artistOpinions :: Map UUID ArtistOpinion,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''UpsertArtistOpinionsCommandResponse

data UpsertArtistOpinionsRequestItem = UpsertArtistOpinionsRequestItem
  { artistIdentifier :: UUID,
    isLike :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype UpsertArtistOpinionsRequest = UpsertArtistOpinionsRequest
  { artistOpinions :: [UpsertArtistOpinionsRequestItem]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''UpsertArtistOpinionsRequest
makeFieldLabelsNoPrefix ''UpsertArtistOpinionsRequestItem

-- artist artworks

data InsertArtistArtworksCommandResponse = InsertArtistArtworksCommandResponse
  { artistArtworks :: Map UUID ArtistArtwork,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertArtistArtworksCommandResponse

data InsertArtistArtworksRequestItem = InsertArtistArtworksRequestItem
  { artistIdentifier :: UUID,
    contentUrl :: Text,
    contentCaption :: Maybe Text,
    orderValue :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertArtistArtworksRequest = InsertArtistArtworksRequest
  { artistArtworks :: [InsertArtistArtworksRequestItem]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertArtistArtworksRequest
makeFieldLabelsNoPrefix ''InsertArtistArtworksRequestItem

newtype ArtistArtworkOrderUpdateRequest = ArtistArtworkOrderUpdateRequest
  { artistArtworkOrders :: [ArtistArtworkOrderUpdate]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''ArtistArtworkOrderUpdateRequest

newtype ArtistDeltaRequest = ArtistDeltaRequest
  { artistDeltas :: [ArtistDelta]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''ArtistDeltaRequest

data ArtistError
  = ValidationFailedError (Map Text ValidationResult)
  | AccessUnauthorizedError
  | SomeError Text
  deriving (Show, Eq, Generic)

ifAllValid ::
  (Applicative f) =>
  Map Text (Validation [Text]) ->
  f (Either ArtistError b) ->
  f (Either ArtistError b)
ifAllValid validationResults eff = do
  if null $ filterFailedValidations validationResults
    then do eff
    else pure . Left $ ValidationFailedError validationResults
