-- Copyright © FINN.no AS, Inc. All rights reserved.

module Unleash.HttpClient (
    getAllClientFeatures,
    getVersion,
    sendMetrics,
    register,
) where

import Data.Aeson (ToJSON, encode)
import Data.Foldable (find)
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map, fromListWith)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import qualified Network.HTTP.Media as M
import Servant.API (Accept (contentTypes), Get, Header, JSON, MimeRender (mimeRender), NoContent, PostNoContent, ReqBody, type (:<|>) (..), type (:>))
import Servant.Client (ClientEnv, ClientError, client, runClientM)
import Unleash.Internal.DomainTypes (Features, fromJsonFeatures, supportedStrategies)
import Unleash.Internal.JsonTypes (FullMetricBucket (..), FullMetricsPayload (..), FullRegisterPayload (..), MetricsPayload, RegisterPayload, YesAndNoes (..))
import qualified Unleash.Internal.JsonTypes as UJT

api :: Proxy Api
api = Proxy

data CustomJSON = CustomJSON

-- Remove charset=utf-8 because older versions of Unleash (e.g. 3.17.4) does not recognize it
instance Accept CustomJSON where
    contentTypes _ =
        "application" M.// "json"
            NE.:| ["application" M.// "json"]

instance {-# OVERLAPPABLE #-} ToJSON a => MimeRender CustomJSON a where
    mimeRender _ = encode

type Api = GetAllClientFeatures :<|> SendMetrics :<|> Register
type GetAllClientFeatures = "api" :> "client" :> "features" :> Header "Authorization" Text :> Get '[JSON] UJT.Features
type SendMetrics = "api" :> "client" :> "metrics" :> Header "Authorization" Text :> ReqBody '[CustomJSON] FullMetricsPayload :> PostNoContent
type Register = "api" :> "client" :> "register" :> Header "Authorization" Text :> Header "Content-Type" Text :> ReqBody '[CustomJSON] FullRegisterPayload :> PostNoContent

getAllClientFeatures' :<|> sendMetrics' :<|> register' = client api

type ApiKey = Text

getAllClientFeatures :: ClientEnv -> Maybe ApiKey -> IO (Either ClientError Features)
getAllClientFeatures clientEnv apiKey = do
    eState <- runClientM (getAllClientFeatures' apiKey) clientEnv
    pure $ fromJsonFeatures <$> eState

sendMetrics :: ClientEnv -> Maybe ApiKey -> MetricsPayload -> IO (Either ClientError NoContent)
sendMetrics clientEnv apiKey metricsPayload = do
    runClientM (sendMetrics' apiKey fullMetricsPayload) clientEnv
    where
        fullMetricsPayload :: FullMetricsPayload
        fullMetricsPayload =
            FullMetricsPayload
                { appName = metricsPayload.appName,
                  instanceId = metricsPayload.instanceId,
                  bucket =
                    FullMetricBucket
                        { start = metricsPayload.start,
                          stop = metricsPayload.stop,
                          toggles = createMapOfYesAndNoes metricsPayload.toggles
                        }
                }

        createMapOfYesAndNoes :: [(Text, Bool)] -> Map Text YesAndNoes
        createMapOfYesAndNoes input = do
            let input' :: [(Text, [Bool])] = (\(k, v) -> (k, [v])) <$> input
            let map :: (Map Text [Bool]) = fromListWith (++) input'

            boolsToYesNo <$> map

        boolsToYesNo :: [Bool] -> YesAndNoes
        boolsToYesNo bools = do
            let yes = length $ filter id bools
            let no = length bools - yes
            YesAndNoes yes no

getVersion :: IO (Maybe Text)
getVersion = do
    cabalFile <- readFile "unleash-client-haskell-core.cabal"
    pure $ fmap pack $ (!! 1) . words <$> (find (isPrefixOf "version") . lines $ cabalFile)

register :: ClientEnv -> Maybe ApiKey -> RegisterPayload -> IO (Either ClientError NoContent)
register clientEnv apiKey registerPayload = do
    version <- getVersion
    let fullRegisterPayload =
            FullRegisterPayload
                { appName = registerPayload.appName,
                  instanceId = registerPayload.instanceId,
                  sdkVersion = "unleash-client-haskell-core:" <> fromJust version,
                  strategies = supportedStrategies,
                  started = registerPayload.started,
                  interval = registerPayload.intervalSeconds * 1000
                }
    runClientM (register' apiKey (Just "application/json") fullRegisterPayload) clientEnv
