-- Copyright © FINN.no AS, Inc. All rights reserved.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Unleash.Internal.JsonTypes where

import Data.Aeson (FromJSON, Options (..), ToJSON (toJSON), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (parseJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Features = Features
    { version :: Int,
      features :: [Feature],
      segments :: Maybe [Segment]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Feature = Feature
    { name :: Text,
      description :: Maybe Text,
      enabled :: Bool,
      strategies :: [Strategy],
      variants :: Maybe [Variant]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Strategy = Strategy
    { name :: Text,
      parameters :: Maybe (Map Text Text),
      constraints :: Maybe [Constraint],
      segments :: Maybe [Int]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

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

data Variant = Variant
    { name :: Text,
      payload :: Maybe Payload,
      weight :: Int,
      stickiness :: Maybe Text,
      overrides :: Maybe [Override]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

typeWorkAroundOptions :: Options
typeWorkAroundOptions =
    defaultOptions {fieldLabelModifier = typeWorkaround}
    where
        typeWorkaround :: String -> String
        typeWorkaround s = case s of
            "type" -> "type_"
            "type_" -> "type"
            _ -> s

data Payload = Payload
    { type_ :: Text,
      value :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Payload where
    parseJSON = genericParseJSON typeWorkAroundOptions

instance ToJSON Payload where
    toJSON = genericToJSON typeWorkAroundOptions

data Override = Override
    { contextName :: Text,
      values :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Context = Context
    { userId :: Maybe Text,
      sessionId :: Maybe Text,
      remoteAddress :: Maybe Text,
      currentTime :: Maybe Text,
      environment :: Maybe Text,
      appName :: Maybe Text,
      properties :: Maybe (Map Text (Maybe Text))
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

emptyContext :: Context
emptyContext = Context Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Segment = Segment
    { id :: Int,
      constraints :: [Constraint]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data VariantResponse = VariantResponse
    { name :: Text,
      payload :: Maybe Payload,
      enabled :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

emptyVariantResponse :: VariantResponse
emptyVariantResponse =
    VariantResponse
        { name = "disabled",
          payload = Nothing,
          enabled = False
        }

data MetricsPayload = MetricsPayload
    { appName :: Text,
      instanceId :: Text,
      start :: UTCTime,
      stop :: UTCTime,
      toggles :: [(Text, Bool)]
    }
    deriving stock (Eq, Show, Generic)

data FullMetricsPayload = FullMetricsPayload
    { appName :: Text,
      instanceId :: Text,
      bucket :: FullMetricBucket
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data FullMetricBucket = FullMetricBucket
    { start :: UTCTime,
      stop :: UTCTime,
      toggles :: Map Text YesAndNoes
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data YesAndNoes = YesAndNoes
    { yes :: Int,
      no :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

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

data RegisterPayload = RegisterPayload
    { appName :: Text,
      instanceId :: Text,
      started :: UTCTime,
      intervalSeconds :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)
