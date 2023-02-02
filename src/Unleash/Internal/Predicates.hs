{- |
Module      : Unleash.Internal.Predicates
Copyright   : Copyright © FINN.no AS, Inc. All rights reserved.
License     : MIT
Stability   : experimental

Predicate helpers.
-}
module Unleash.Internal.Predicates (
    datePredicate,
    numPredicate,
    semVerPredicate,
) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (ZonedTime)
import Data.Versions (SemVer, semver)
import Text.Read (readMaybe)

-- | Evaluate a predicate for UTC times.
datePredicate ::
    -- | Predicate.
    (UTCTime -> UTCTime -> Bool) ->
    -- | First argument to predicate (to be parsed to UTC time).
    Maybe Text ->
    -- | Second argument to predicate (to be parsed to UTC time).
    Maybe Text ->
    -- | Predicate result.
    Bool
datePredicate predicate mCurrentValue mConstraintValue = do
    let parseDate :: Text -> Maybe UTCTime
        parseDate text =
            (iso8601ParseM @Maybe @UTCTime $ Text.unpack text)
                <|> zonedTimeToUTC <$> (iso8601ParseM @Maybe @ZonedTime $ Text.unpack text)

    let mConstraintDate :: Maybe UTCTime = parseDate =<< mConstraintValue
    let mProvidedDate :: Maybe UTCTime = parseDate =<< mCurrentValue

    case (mProvidedDate, mConstraintDate) of
        (Just providedDate, Just constraintDate) -> predicate providedDate constraintDate
        _ -> False

-- | Evaluate a predicate for numbers.
numPredicate ::
    -- | Predicate.
    (Double -> Double -> Bool) ->
    -- | First argument to predicate (to be parsed to UTC time).
    Maybe Text ->
    -- | Second argument to predicate (to be parsed to UTC time).
    Maybe Text ->
    -- | Predicate result.
    Bool
numPredicate predicate mCurrentValue mConstraintValue = do
    let maybeCurrentValue :: Maybe Double = readMaybe . Text.unpack =<< mCurrentValue
    let maybeConstraintValue :: Maybe Double = readMaybe . Text.unpack =<< mConstraintValue

    case (maybeConstraintValue, maybeCurrentValue) of
        (Just constraintValue, Just currentValue) -> predicate currentValue constraintValue
        _ -> False

-- | Evaluate a predicate for semantic versioning values.
semVerPredicate ::
    -- | Predicate.
    (SemVer -> SemVer -> Bool) ->
    -- | First argument to predicate (to be parsed to a version).
    Maybe Text ->
    -- | Second argument to predicate (to be parsed to a version).
    Maybe Text ->
    -- | Predicate result.
    Bool
semVerPredicate predicate mCurrentValue mConstraintValue = do
    let eitherToMaybe e =
            case e of
                Right a -> Just a
                _ -> Nothing

    let mConstraintSemVer :: Maybe SemVer = eitherToMaybe . semver =<< mConstraintValue
    let mProvidedSemVer :: Maybe SemVer = eitherToMaybe . semver =<< mCurrentValue

    case (mProvidedSemVer, mConstraintSemVer) of
        (Just providedSemVer, Just constraintSemVer) -> predicate providedSemVer constraintSemVer
        _ -> False
