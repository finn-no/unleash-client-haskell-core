-- Copyright © FINN.no AS, Inc. All rights reserved.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module UnleashSpecificationJsonTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Unleash.Internal.JsonTypes (Context, Features, VariantResponse)

data Specification = Specification
    { name :: Text,
      state :: Features,
      tests :: Maybe [Test],
      variantTests :: Maybe [VariantTest]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Test = Test
    { description :: Text,
      context :: Context,
      toggleName :: Text,
      expectedResult :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data VariantTest = VariantTest
    { description :: Text,
      context :: Context,
      toggleName :: Text,
      expectedResult :: VariantResponse
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
