{- |
Module      : Unleash
Copyright   : Copyright © FINN.no AS, Inc. All rights reserved.
License     : MIT
Stability   : experimental

Functions and types for checking feature toggles and variants.
-}
module Unleash (
    Context (..),
    defaultStrategyEvaluator,
    defaultSupportedStrategies,
    emptyContext,
    emptyVariantResponse,
    featureGetVariant,
    featureIsEnabled,
    Features,
    FeatureToggleName,
    MetricsPayload (..),
    RegisterPayload (..),
    Strategy (..),
    StrategyEvaluator,
    SupportedStrategies,
    VariantResponse (..),
) where

import Unleash.Internal.DomainTypes (
    FeatureToggleName,
    Features,
    StrategyEvaluator,
    defaultStrategyEvaluator,
    defaultSupportedStrategies,
    featureGetVariant,
    featureIsEnabled,
 )
import Unleash.Internal.JsonTypes (
    Context (..),
    MetricsPayload (..),
    RegisterPayload (..),
    Strategy (..),
    SupportedStrategies,
    VariantResponse (..),
    emptyContext,
    emptyVariantResponse,
 )
