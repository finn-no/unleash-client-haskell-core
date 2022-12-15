-- Copyright © FINN.no AS, Inc. All rights reserved.

module VersionSpec (
    spec,
) where

import Data.Maybe (isJust)
import Test.Hspec
import Unleash.HttpClient (getVersion)

spec :: Spec
spec = do
    describe "Version" do
        it "Get correct version from Cabal file" do
            version <- getVersion
            version `shouldSatisfy` isJust
