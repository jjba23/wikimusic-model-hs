{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Model.Thread
  ( Thread (..),
    ThreadRender (..),
    mkThreads,
    renderThread,
  )
where

import Data.Aeson
import Data.OpenApi
import Optics
import Relude

data Thread a = Thread a [Thread a]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ThreadRender a = ThreadRender
  { node :: a,
    subNodes :: [ThreadRender a]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance (ToSchema a) => ToSchema (ThreadRender a)

makeFieldLabelsNoPrefix ''Thread

renderThread :: Thread a -> ThreadRender a
renderThread (Thread x xs) = do
  ThreadRender {node = x, subNodes = map renderThread xs}

mkThreads :: [a] -> (a -> a -> Bool) -> (a -> Maybe b) -> [Thread a]
mkThreads nodes isChildOf getParentIdentifier = filterRootNodes $ map withThreadChildren nodes
  where
    directChildren node = filter (isChildOf node) nodes
    withThreadChildren node = Thread node $ map withThreadChildren (directChildren node)
    filterRootNodes = filter (\(Thread x _) -> isNothing (getParentIdentifier x))
