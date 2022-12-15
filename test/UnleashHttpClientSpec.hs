-- Copyright © FINN.no AS, Inc. All rights reserved.

module UnleashHttpClientSpec (
    spec,
) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (..), Scheme (Http), mkClientEnv)
import Test.Hspec
import Unleash.HttpClient (getAllClientFeatures)
import Unleash.Internal.DomainTypes (featureIsEnabled)
import Unleash.Internal.JsonTypes (emptyContext)

unleashUrl = BaseUrl Http "my-local-unleash" 80 mempty
testFeatureName = "my-feature"

spec :: Spec
spec = do
    describe "Use Unleash API" do
        xit "get all client features" do
            manager <- newManager defaultManagerSettings
            let clientEnv = mkClientEnv manager unleashUrl
            (Right state) <- getAllClientFeatures clientEnv Nothing
            actualEnabled <- featureIsEnabled state testFeatureName emptyContext
            actualEnabled `shouldBe` True
