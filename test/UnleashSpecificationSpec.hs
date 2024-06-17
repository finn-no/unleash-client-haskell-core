-- Copyright © FINN.no AS, Inc. All rights reserved.

module UnleashSpecificationSpec (
    spec,
) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Test.Hspec
import Unleash.Internal.DomainTypes (defaultStrategyEvaluator, featureGetVariant, featureIsEnabled, fromJsonFeatures)
import qualified UnleashSpecificationJsonTypes as JsonTypes

spec :: Spec
spec =
    describe "Run all specifications" do
        it "run all specification files" do
            Right allFiles <- eitherDecodeFileStrict @([FilePath]) "./client-specification/specifications/index.json"
            let exclusions = ["16-strategy-variants.json", "17-dependent-features.json"]
            let files = filter (not . \path -> any ((==) path) exclusions) allFiles
            let paths = ("./client-specification/specifications/" <>) <$> files
            traverse_ runSpecification paths

runSpecification :: FilePath -> Expectation
runSpecification filePath = do
    Right specification <- eitherDecodeFileStrict @JsonTypes.Specification filePath

    let state = fromJsonFeatures defaultStrategyEvaluator specification.state

    let isEnabled' :: JsonTypes.Test -> Expectation
        isEnabled' sut = do
            print sut.description
            actual <- featureIsEnabled state sut.toggleName sut.context
            actual `shouldBe` sut.expectedResult

    traverse_ isEnabled' $ fromMaybe [] specification.tests

    let getVariant' :: JsonTypes.VariantTest -> Expectation
        getVariant' sut = do
            print sut.description
            variantResponse <- featureGetVariant state sut.toggleName sut.context
            variantResponse `shouldBe` sut.expectedResult

    traverse_ getVariant' $ fromMaybe [] specification.variantTests
