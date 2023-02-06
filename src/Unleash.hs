{- |
Module      : Unleash
Copyright   : Copyright © FINN.no AS, Inc. All rights reserved.
License     : MIT
Stability   : experimental

Functions and types for checking feature toggles and variants.
-}
module Unleash (
    Features,
    Context (..),
    emptyContext,
    emptyVariantResponse,
    featureIsEnabled,
    featureGetVariant,
    MetricsPayload (..),
    RegisterPayload (..),
    VariantResponse (..),
) where

import Unleash.Internal.DomainTypes (Features, featureGetVariant, featureIsEnabled)
import Unleash.Internal.JsonTypes (Context (..), MetricsPayload (..), RegisterPayload (..), VariantResponse (..), emptyContext, emptyVariantResponse)
