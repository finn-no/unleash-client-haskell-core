{- |
Module      : Unleash.Internal.DomainTypes
Copyright   : Copyright © FINN.no AS, Inc. All rights reserved.
License     : MIT
Stability   : experimental

Domain types and evaluation functions.
-}
module Unleash.Internal.DomainTypes (
    featureGetVariant,
    featureIsEnabled,
    fromJsonFeatures,
    supportedStrategies,
    Feature (..),
    Features,
    FeatureToggleName,
    GetVariant (..),
    IsEnabled (..),
) where
import Control.Applicative (liftA2, (<|>))
import Control.Monad.IO.Class (MonadIO)
import Data.Hash.Murmur (murmur3)
import Data.List (find)
import Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import System.Random (randomRIO)
import TextShow (showt)
import Unleash.Internal.JsonTypes (Variant, VariantResponse (..), emptyVariantResponse)
import qualified Unleash.Internal.JsonTypes as JsonTypes
import Unleash.Internal.Predicates (datePredicate, numPredicate, semVerPredicate)

-- | A list of currently supported strategies for this library.
supportedStrategies :: [Text]
supportedStrategies = ["default", "userWithId", "gradualRolloutUserId", "gradualRolloutSessionId", "gradualRolloutRandom", "remoteAddress", "flexibleRollout"]

-- | Alias used for feature toggle names (as they are represented on Unleash servers).
type FeatureToggleName = Text

-- | Map of feature toggles keyed on toggle names. Typically the full set of features fetched from a server.
type Features = Map FeatureToggleName Feature

-- | Map of feature toggles keyed on strategy parameters.
type Parameters = Map Text FeatureToggleName

-- | Feature toggle state getter.
newtype IsEnabled = IsEnabled (forall m. MonadIO m => JsonTypes.Context -> m Bool)

-- | Feature toggle variant getter.
newtype GetVariant = GetVariant (forall m. MonadIO m => JsonTypes.Context -> m VariantResponse)

-- | Feature toggle.
data Feature = Feature
    { -- | Feature toggle state getter.
      isEnabled :: IsEnabled,
      -- | Feature toggle variant getter.
      getVariant :: GetVariant
    }

segmentMap :: Maybe [JsonTypes.Segment] -> Map Int [JsonTypes.Constraint]
segmentMap maybeSegments =
    let segments :: [JsonTypes.Segment] = concat maybeSegments
     in fromList $ (\segment -> (segment.id, segment.constraints)) <$> segments

-- | Feature toggle set domain transfer object to domain type converter.
fromJsonFeatures :: JsonTypes.Features -> Features
fromJsonFeatures jsonFeatures = fromList $ fmap (fromJsonFeature (segmentMap jsonFeatures.segments)) jsonFeatures.features

generateRandomText :: MonadIO m => m Text
generateRandomText = showt <$> randomRIO @Int (0, 99999)

fromJsonFeature :: Map Int [JsonTypes.Constraint] -> JsonTypes.Feature -> (FeatureToggleName, Feature)
fromJsonFeature segmentMap jsonFeature =
    ( jsonFeature.name,
      Feature
        { isEnabled = IsEnabled $ \ctx -> do
            isAnyStrategyEnabled <- anyStrategyEnabled ctx
            pure $ jsonFeature.enabled && (null jsonFeature.strategies || isAnyStrategyEnabled),
          getVariant = GetVariant $ \ctx ->
            if not jsonFeature.enabled
                then pure emptyVariantResponse
                else do
                    let variants :: [Variant] = fromMaybe [] jsonFeature.variants
                    case enabledByOverride variants ctx of
                        Just variant ->
                            -- Has overrides
                            pure $
                                VariantResponse
                                    { name = variant.name,
                                      payload = variant.payload,
                                      enabled = True
                                    }
                        Nothing -> do
                            -- Does not have overrides
                            let maybeStickiness = find ("default" /=) . catMaybes $ (.stickiness) <$> variants
                            case maybeStickiness of
                                Just stickiness -> do
                                    -- Has non-default stickiness
                                    let identifier = lookupContextValue stickiness ctx
                                    selectVariant variants identifier jsonFeature.name
                                Nothing -> do
                                    -- Default stickiness
                                    let identifier = ctx.userId <|> ctx.sessionId <|> ctx.remoteAddress
                                    selectVariant variants identifier jsonFeature.name
        }
    )
    where
        anyStrategyEnabled :: MonadIO m => JsonTypes.Context -> m Bool
        anyStrategyEnabled ctx = or <$> traverse (\f -> f ctx) strategyPredicates

        strategyPredicates :: MonadIO m => [JsonTypes.Context -> m Bool]
        strategyPredicates =
            fmap (fromJsonStrategy jsonFeature.name segmentMap) jsonFeature.strategies

        enabledByOverride :: [Variant] -> JsonTypes.Context -> Maybe Variant
        enabledByOverride variants ctx =
            find
                ( \variant -> case variant.overrides of
                    Nothing -> False
                    Just overrides ->
                        any
                            ( \override ->
                                lookupContextValue override.contextName ctx `elem` (Just <$> override.values)
                            )
                            overrides
                )
                variants

        selectVariant :: MonadIO m => [Variant] -> Maybe Text -> Text -> m VariantResponse
        selectVariant variants maybeIdentifier featureName = do
            randomValue <- generateRandomText
            let identifier = fromMaybe randomValue maybeIdentifier
                weights = (.weight) <$> variants
                hashed = getNormalizedNumberN identifier featureName (fromIntegral $ sum weights)
                accumulated = tail $ scanl (+) 0 weights
                zipped = zip accumulated variants
                maybeVariant = snd <$> find (\(acc, _) -> acc >= hashed) zipped
             in case maybeVariant of
                    Nothing -> pure emptyVariantResponse
                    Just variant ->
                        pure $
                            VariantResponse
                                { name = variant.name,
                                  payload = variant.payload,
                                  enabled = True
                                }

fromJsonStrategy :: MonadIO m => FeatureToggleName -> Map Int [JsonTypes.Constraint] -> JsonTypes.Strategy -> (JsonTypes.Context -> m Bool)
fromJsonStrategy featureToggleName segmentMap jsonStrategy =
    \ctx -> liftA2 (&&) (strategyFunction ctx) (constraintsPredicate ctx)
    where
        strategyFunction :: MonadIO m => JsonTypes.Context -> m Bool
        strategyFunction =
            case jsonStrategy.name of
                "default" -> pure . \_ctx -> True
                "userWithId" ->
                    pure . \ctx ->
                        let strategy params =
                                let userIds = maybe [] splitParams (Map.lookup "userIds" params)
                                 in ctx.userId `elem` (Just <$> userIds)
                         in evaluateStrategy strategy jsonStrategy.parameters
                "gradualRolloutUserId" ->
                    pure . \ctx ->
                        case ctx.userId of
                            Nothing -> False
                            Just userId ->
                                evaluateStrategy strategy jsonStrategy.parameters
                                where
                                    strategy params =
                                        let percentage = getInt "percentage" params
                                            groupId = fromMaybe featureToggleName $ Map.lookup "groupId" params
                                            normValue = getNormalizedNumber userId groupId
                                         in normValue <= percentage
                "gradualRolloutSessionId" ->
                    pure . \ctx ->
                        case ctx.sessionId of
                            Nothing -> False
                            Just sessionId ->
                                evaluateStrategy strategy jsonStrategy.parameters
                                where
                                    strategy params =
                                        let percentage = getInt "percentage" params
                                            groupId = fromMaybe featureToggleName $ Map.lookup "groupId" params
                                            normValue = getNormalizedNumber sessionId groupId
                                         in normValue <= percentage
                "gradualRolloutRandom" -> \_ctx -> do
                    case jsonStrategy.parameters of
                        Nothing -> pure False
                        Just params -> do
                            let percentage = getInt "percentage" params
                            num <- randomRIO @Int (1, 100)
                            pure $ percentage >= num
                "remoteAddress" ->
                    pure . \ctx ->
                        let strategy params =
                                let remoteAddresses = maybe [] splitParams (Map.lookup "IPs" params)
                                 in ctx.remoteAddress `elem` (Just <$> remoteAddresses)
                         in evaluateStrategy strategy jsonStrategy.parameters
                "flexibleRollout" -> \ctx -> do
                    randomValue <- generateRandomText
                    let strategy params =
                            let rollout = getInt "rollout" params
                                stickiness = fromMaybe "default" $ Map.lookup "stickiness" params
                                groupId = fromMaybe featureToggleName $ Map.lookup "groupId" params
                             in case stickiness of
                                    "default" ->
                                        normalizedNumber <= rollout
                                        where
                                            identifier = fromMaybe randomValue (ctx.userId <|> ctx.sessionId <|> ctx.remoteAddress)
                                            normalizedNumber = getNormalizedNumber identifier groupId
                                    "userId" ->
                                        case ctx.userId of
                                            Nothing -> False
                                            Just userId -> getNormalizedNumber userId groupId <= rollout
                                    "sessionId" ->
                                        case ctx.sessionId of
                                            Nothing -> False
                                            Just sessionId -> getNormalizedNumber sessionId groupId <= rollout
                                    customField ->
                                        case lookupContextValue customField ctx of
                                            Nothing -> False
                                            Just customValue ->
                                                getNormalizedNumber customValue groupId <= rollout
                     in pure $ evaluateStrategy strategy jsonStrategy.parameters
                -- Unknown strategy
                _ -> pure . \_ctx -> False
            where
                splitParams :: Text -> [Text]
                splitParams = fmap Text.strip . Text.splitOn ","

        segmentsToConstraints :: [Int] -> Map Int [JsonTypes.Constraint] -> [Maybe JsonTypes.Constraint]
        segmentsToConstraints segmentReferences segmentMap =
            concat $ sequence <$> ((flip Map.lookup) segmentMap <$> segmentReferences)

        constraintsPredicate :: MonadIO m => JsonTypes.Context -> m Bool
        constraintsPredicate ctx = do
            let segmentReferences = concat jsonStrategy.segments
                maybeSegmentConstraints = segmentsToConstraints segmentReferences segmentMap
                segmentConstraints = catMaybes maybeSegmentConstraints
                strategyConstraints = fromMaybe [] jsonStrategy.constraints
                allConstraints = segmentConstraints <> strategyConstraints
                allPredicates = fromJsonConstraint <$> allConstraints
                allSegmentConstraintsAreReferredTo = not $ Nothing `elem` maybeSegmentConstraints
                allPredicatesAreSatisfied = allSegmentConstraintsAreReferredTo && and (evaluatePredicate <$> allPredicates)
                thereAreNoPredicates = null allPredicates
            pure $ thereAreNoPredicates || allPredicatesAreSatisfied
            where
                evaluatePredicate :: (JsonTypes.Context -> Bool) -> Bool
                evaluatePredicate f = f ctx

fromJsonConstraint :: JsonTypes.Constraint -> (JsonTypes.Context -> Bool)
fromJsonConstraint constraint = \ctx -> do
    let constraintValues =
            if fromMaybe False constraint.caseInsensitive
                then Text.toLower <$> fromMaybe [] constraint.values
                else fromMaybe [] constraint.values

    let mCurrentValue = do
            let tmpValue :: Maybe Text = lookupContextValue constraint.contextName ctx
            if fromMaybe False constraint.caseInsensitive
                then (Text.toLower <$> tmpValue)
                else tmpValue

    let result =
            case constraint.operator of
                "IN" -> mCurrentValue `isIn` constraintValues
                "NOT_IN" -> mCurrentValue `isNotIn` constraintValues
                "STR_STARTS_WITH" -> mCurrentValue `startsWithAnyOf` constraintValues
                "STR_ENDS_WITH" -> mCurrentValue `endsWithAnyOf` constraintValues
                "STR_CONTAINS" -> mCurrentValue `containsAnyOf` constraintValues
                "NUM_EQ" -> numPredicate (==) mCurrentValue constraint.value
                "NUM_GT" -> numPredicate (>) mCurrentValue constraint.value
                "NUM_GTE" -> numPredicate (>=) mCurrentValue constraint.value
                "NUM_LTE" -> numPredicate (<=) mCurrentValue constraint.value
                "NUM_LT" -> numPredicate (<) mCurrentValue constraint.value
                "DATE_AFTER" -> datePredicate (>) mCurrentValue constraint.value
                "DATE_BEFORE" -> datePredicate (<) mCurrentValue constraint.value
                "SEMVER_EQ" -> semVerPredicate (==) mCurrentValue constraint.value
                "SEMVER_GT" -> semVerPredicate (>) mCurrentValue constraint.value
                "SEMVER_LT" -> semVerPredicate (<) mCurrentValue constraint.value
                _ -> False

    if fromMaybe False constraint.inverted
        then not result
        else result

lookupContextValue :: Text -> JsonTypes.Context -> Maybe Text
lookupContextValue key ctx =
    case key of
        "appName" -> ctx.appName
        "currentTime" -> ctx.currentTime
        "environment" -> ctx.environment
        "remoteAddress" -> ctx.remoteAddress
        "sessionId" -> ctx.sessionId
        "userId" -> ctx.userId
        propertiesKey -> do
            m <- ctx.properties
            value <- Map.lookup propertiesKey m
            value

isIn :: Eq a => Maybe a -> [a] -> Bool
isIn mCurrentValue values =
    case mCurrentValue of
        Nothing -> False
        Just currentValue -> currentValue `elem` values

isNotIn :: Eq a => Maybe a -> [a] -> Bool
isNotIn mCurrentValue values = not $ isIn mCurrentValue values

startsWithAnyOf :: Maybe Text -> [Text] -> Bool
startsWithAnyOf mCurrentValue values = do
    case mCurrentValue of
        Nothing -> False
        Just currentValue -> any (`Text.isPrefixOf` currentValue) values

endsWithAnyOf :: Maybe Text -> [Text] -> Bool
endsWithAnyOf mCurrentValue values = do
    case mCurrentValue of
        Nothing -> False
        Just currentValue -> any (`Text.isSuffixOf` currentValue) values

containsAnyOf :: Maybe Text -> [Text] -> Bool
containsAnyOf mCurrentValue values = do
    case mCurrentValue of
        Nothing -> False
        Just currentValue -> any (`Text.isInfixOf` currentValue) values

getNormalizedNumberN :: Text -> Text -> Word32 -> Int
getNormalizedNumberN identifier groupId n = do
    let s = groupId <> ":" <> identifier
    let hash :: Word32 = murmur3 (0 :: Word32) $ encodeUtf8 s

    fromIntegral $ (mod hash n) + 1

getNormalizedNumber :: Text -> Text -> Int
getNormalizedNumber identifier groupId = getNormalizedNumberN identifier groupId 100

-- | Check whether or not a feature toggle is enabled.
featureIsEnabled ::
    MonadIO m =>
    -- | Full set of features fetched from a server.
    Features ->
    -- | Feature toggle name (as it is represented on the server).
    FeatureToggleName ->
    -- | User context.
    JsonTypes.Context ->
    -- | Feature toggle state.
    m Bool
featureIsEnabled state toggleName ctx = do
    let mToggle :: Maybe Feature = Map.lookup toggleName state
    case mToggle of
        Just Feature {isEnabled = IsEnabled isEnabled} -> isEnabled ctx
        Nothing -> pure False

getInt :: Text -> Parameters -> Int
getInt key params = read . Text.unpack $ fromMaybe "0" (Map.lookup key params)

evaluateStrategy :: (a -> Bool) -> Maybe a -> Bool
evaluateStrategy f p = maybe False f p

-- | Get a variant for a given feature toggle.
featureGetVariant ::
    MonadIO m =>
    -- | Full set of features fetched from a server.
    Features ->
    -- | Feature toggle name (as it is represented on the server).
    FeatureToggleName ->
    -- | User context.
    JsonTypes.Context ->
    -- | Variant.
    m VariantResponse
featureGetVariant state toggleName ctx = do
    let mToggle :: Maybe Feature = Map.lookup toggleName state
    case mToggle of
        Just Feature {getVariant = GetVariant getVariant} -> getVariant ctx
        Nothing -> pure emptyVariantResponse
