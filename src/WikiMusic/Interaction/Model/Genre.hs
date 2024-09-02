{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Interaction.Model.Genre
  ( GenreError (..),
    ifAllValid,
    Genre (..),
    GenreArtwork (..),
    GenreComment (..),
    GenreOpinion (..),
    GetGenresQueryResponse (..),
    InsertGenresCommandResponse (..),
    InsertGenresRequest (..),
    InsertGenresRequestItem (..),
    InsertGenreCommentsCommandResponse (..),
    InsertGenreCommentsRequest (..),
    InsertGenreCommentsRequestItem (..),
    UpsertGenreOpinionsCommandResponse (..),
    UpsertGenreOpinionsRequest (..),
    UpsertGenreOpinionsRequestItem (..),
    InsertGenreArtworksCommandResponse (..),
    InsertGenreArtworksRequest (..),
    InsertGenreArtworksRequestItem (..),
    GenreArtworkOrderUpdateRequest (..),
    GenreDeltaRequest (..),
  )
where

import Data.Aeson hiding (Success)
import Data.OpenApi
import Data.UUID hiding (null)
import Keuringsdienst
import Keuringsdienst.Helpers
import Optics
import Relude
import WikiMusic.Model.Genre

instance ToSchema (Validation [Text])

data GetGenresQueryResponse = GetGenresQueryResponse
  { genres :: Map UUID Genre,
    sortOrder :: [UUID]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''GetGenresQueryResponse

data InsertGenresCommandResponse = InsertGenresQueryResponse
  { genres :: Map UUID Genre,
    sortOrder :: [UUID],
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertGenresCommandResponse

data InsertGenresRequestItem = InsertGenresRequestItem
  { displayName :: Text,
    spotifyUrl :: Maybe Text,
    youtubeUrl :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    wikipediaUrl :: Maybe Text,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertGenresRequest = InsertGenresRequest
  { genres :: [InsertGenresRequestItem]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertGenresRequest
makeFieldLabelsNoPrefix ''InsertGenresRequestItem

-- genre comments

data InsertGenreCommentsCommandResponse = InsertGenreCommentsCommandResponse
  { genreComments :: Map UUID GenreComment,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertGenreCommentsCommandResponse

data InsertGenreCommentsRequestItem = InsertGenreCommentsRequestItem
  { genreIdentifier :: UUID,
    parentIdentifier :: Maybe UUID,
    contents :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertGenreCommentsRequest = InsertGenreCommentsRequest
  { genreComments :: [InsertGenreCommentsRequestItem]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertGenreCommentsRequest
makeFieldLabelsNoPrefix ''InsertGenreCommentsRequestItem

-- genre opinions
data UpsertGenreOpinionsCommandResponse = UpsertGenreOpinionsCommandResponse
  { genreOpinions :: Map UUID GenreOpinion,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''UpsertGenreOpinionsCommandResponse

data UpsertGenreOpinionsRequestItem = UpsertGenreOpinionsRequestItem
  { genreIdentifier :: UUID,
    isLike :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype UpsertGenreOpinionsRequest = UpsertGenreOpinionsRequest
  { genreOpinions :: [UpsertGenreOpinionsRequestItem]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''UpsertGenreOpinionsRequest
makeFieldLabelsNoPrefix ''UpsertGenreOpinionsRequestItem

-- genre artworks

data InsertGenreArtworksCommandResponse = InsertGenreArtworksCommandResponse
  { genreArtworks :: Map UUID GenreArtwork,
    validationResults :: Map Text ValidationResult
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertGenreArtworksCommandResponse

data InsertGenreArtworksRequestItem = InsertGenreArtworksRequestItem
  { genreIdentifier :: UUID,
    contentUrl :: Text,
    contentCaption :: Maybe Text,
    orderValue :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype InsertGenreArtworksRequest = InsertGenreArtworksRequest
  { genreArtworks :: [InsertGenreArtworksRequestItem]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''InsertGenreArtworksRequest
makeFieldLabelsNoPrefix ''InsertGenreArtworksRequestItem

newtype GenreArtworkOrderUpdateRequest = GenreArtworkOrderUpdateRequest
  { genreArtworkOrders :: [GenreArtworkOrderUpdate]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''GenreArtworkOrderUpdateRequest

newtype GenreDeltaRequest = GenreDeltaRequest
  { genreDeltas :: [GenreDelta]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

makeFieldLabelsNoPrefix ''GenreDeltaRequest

data GenreError
  = ValidationFailedError (Map Text ValidationResult)
  | AccessUnauthorizedError
  | SomeError Text
  deriving (Show, Eq, Generic)

ifAllValid ::
  (Applicative f) =>
  Map Text (Validation [Text]) ->
  f (Either GenreError b) ->
  f (Either GenreError b)
ifAllValid validationResults eff = do
  if null $ filterFailedValidations validationResults
    then do eff
    else pure . Left $ ValidationFailedError validationResults
