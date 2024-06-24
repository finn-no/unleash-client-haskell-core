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
import Data.List (isPrefixOf)
import Test.Hspec
import UnleashSpecificationJsonTypes (Specification)

spec :: Spec
spec =
    describe "Parse json specification" do
        it "parse all specification files and check if input json is the same as serialized json" do
            Right filepaths <- eitherDecodeFileStrict @([FilePath]) "./client-specification/specifications/index.json"

            -- Test 13 contains null as a value that is deleted by WithoutNothing
            let filepathsWO13 = filter (not . isPrefixOf "13") filepaths

            let filepaths' = ("./client-specification/specifications/" <>) <$> filepathsWO13
            traverse_ roundtrip filepaths'

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
