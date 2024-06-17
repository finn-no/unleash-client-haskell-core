-- Copyright © FINN.no AS, Inc. All rights reserved.

module UnleashClientSpecificationRoundtripSpec (
    spec,
) where

import Data.Aeson (ToJSON (toJSON), eitherDecodeFileStrict, eitherDecodeStrict)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap (mapMaybe)
import qualified Data.Aeson.Parser as Parser
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (traverse_)
import Test.Hspec
import UnleashSpecificationJsonTypes (Specification)

spec :: Spec
spec =
    describe "Parse JSON specification" do
        it "parse all specification files and check if input JSON is the same as serialized JSON" do
            Right allFiles <- eitherDecodeFileStrict @([FilePath]) "./client-specification/specifications/index.json"
            -- Test 13 contains null as a value that is deleted by WithoutNothing
            let exclusions = ["13-constraint-operators.json", "16-strategy-variants.json", "17-dependent-features.json"]
            let files = filter (not . \path -> any ((==) path) exclusions) allFiles
            let paths = ("./client-specification/specifications/" <>) <$> files
            traverse_ roundtrip paths

roundtrip :: FilePath -> Expectation
roundtrip filePath = do
    putStrLn ("    " <> filePath)
    bs <- BS.readFile filePath
    let eJsonValue = parseOnly Parser.value bs
    case eJsonValue of
        Right _ -> pure ()
        Left e -> print e
    let Right jsonValue = eJsonValue
    let prettyJson = encodePretty jsonValue

    let eSpecification = eitherDecodeStrict @Specification bs
    case eSpecification of
        Right _ -> pure ()
        Left e -> print e
    let Right specification = eSpecification

    BSL.writeFile "actual.json" $ encodePretty specification
    BSL.writeFile "expected.json" prettyJson
    encodePretty (WithoutNothing specification) `shouldBe` prettyJson

removeUndefinedProps :: Aeson.Value -> Aeson.Value
removeUndefinedProps v =
    case v of
        Aeson.Array v' -> Aeson.Array $ removeUndefinedProps <$> v'
        Aeson.Object km ->
            Aeson.Object $
                mapMaybe
                    ( \v' -> case v' of
                        Aeson.Null -> Nothing
                        _ -> Just $ removeUndefinedProps v'
                    )
                    km
        _ -> v

newtype WithoutNothing a = WithoutNothing a

instance (ToJSON a) => ToJSON (WithoutNothing a) where
    toJSON (WithoutNothing a) = removeUndefinedProps $ toJSON a
