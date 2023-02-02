{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Unleash.Internal.JsonTypes
Copyright   : Copyright © FINN.no AS, Inc. All rights reserved.
License     : MIT
Stability   : experimental

Unleash domain transfer objects.
-}
module Unleash.Internal.JsonTypes where

import Data.Aeson (FromJSON, Options (..), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (parseJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Feature toggle set DTO.
data Features = Features
    { version :: Int,
      features :: [Feature],
      segments :: Maybe [Segment]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Feature toggle DTO.
data Feature = Feature
    { name :: Text,
      description :: Maybe Text,
      enabled :: Bool,
      strategies :: [Strategy],
      variants :: Maybe [Variant]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

{- | Strategy DTO.
 Encompasses all (supported) types of strategies.
-}
data Strategy = Strategy
    { name :: Text,
      parameters :: Maybe (Map Text Text),
      constraints :: Maybe [Constraint],
      segments :: Maybe [Int]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Strategy constraint DTO.
data Constraint = Constraint
    { contextName :: Text,
      operator :: Text,
      values :: Maybe [Text],
      caseInsensitive :: Maybe Bool,
      inverted :: Maybe Bool,
      value :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Variant DTO.
data Variant = Variant
    { name :: Text,
      payload :: Maybe Payload,
      weight :: Int,
      stickiness :: Maybe Text,
      overrides :: Maybe [Override]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A workaround for fields named @type@, which is in conflict with the @type@ keyword.
typeWorkAroundOptions :: Options
typeWorkAroundOptions =
    defaultOptions {fieldLabelModifier = typeWorkaround}
    where
        typeWorkaround :: String -> String
        typeWorkaround s = case s of
            "type" -> "type_"
            "type_" -> "type"
            _ -> s

-- | Variant payload DTO.
data Payload = Payload
    { type_ :: Text,
      value :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Payload where
    parseJSON = genericParseJSON typeWorkAroundOptions

instance ToJSON Payload where
    toJSON = genericToJSON typeWorkAroundOptions

-- | Override DTO.
data Override = Override
    { contextName :: Text,
      values :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Client context DTO.
data Context = Context
    { -- | User ID.
      userId :: Maybe Text,
      -- | Session ID.
      sessionId :: Maybe Text,
      -- | Remote address.
      remoteAddress :: Maybe Text,
      -- | Current UTC time.
      currentTime :: Maybe Text,
      -- | Application environment (e.g. @Production@).
      environment :: Maybe Text,
      -- | Application name.
      appName :: Maybe Text,
      -- | Other custom properties.
      properties :: Maybe (Map Text (Maybe Text))
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | An initial client context.
emptyContext :: Context
emptyContext = Context Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Segment DTO.
data Segment = Segment
    { id :: Int,
      constraints :: [Constraint]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Variant DTO.
data VariantResponse = VariantResponse
    { -- | Variant name.
      name :: Text,
      -- | Variant payload.
      payload :: Maybe Payload,
      -- | Variant state.
      enabled :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | The default (disabled) variant response.
emptyVariantResponse :: VariantResponse
emptyVariantResponse =
    VariantResponse
        { name = "disabled",
          payload = Nothing,
          enabled = False
        }

-- | Metrics payload DTO subset helper.
data MetricsPayload = MetricsPayload
    { -- | Application name.
      appName :: Text,
      -- | Instance identifier (typically hostname).
      instanceId :: Text,
      -- | Start timestamp for this interval.
      start :: UTCTime,
      -- | End timestamp for this interval.
      stop :: UTCTime,
      -- | Feature toggle usage metrics.
      toggles :: [(Text, Bool)]
    }
    deriving stock (Eq, Show, Generic)

-- | Metrics payload DTO.
data FullMetricsPayload = FullMetricsPayload
    { appName :: Text,
      instanceId :: Text,
      bucket :: FullMetricBucket
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Metrics bucket DTO.
data FullMetricBucket = FullMetricBucket
    { -- | Start timestamp for this interval.
      start :: UTCTime,
      -- | End timestamp for this interval.
      stop :: UTCTime,
      -- | Feature toggle usage metrics.
      toggles :: Map Text YesAndNoes
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Helper data structure for metrics.
data YesAndNoes = YesAndNoes
    { -- | The number of times the feature toggle was fetched as enabled in an interval.
      yes :: Int,
      -- | The number of times the feature toggle was fetched as disabled in an interval.
      no :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Registration DTO.
data FullRegisterPayload = FullRegisterPayload
    { appName :: Text,
      instanceId :: Text,
      sdkVersion :: Text,
      strategies :: [Text],
      started :: UTCTime,
      interval :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Client registration DTO subset helper.
data RegisterPayload = RegisterPayload
    { -- | Application name.
      appName :: Text,
      -- | Instance identifier (typically hostname).
      instanceId :: Text,
      -- | Client application startup timestamp.
      started :: UTCTime,
      -- | Intended metrics sending interval.
      intervalSeconds :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)
