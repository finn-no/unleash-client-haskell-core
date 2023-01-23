-- Copyright © FINN.no AS, Inc. All rights reserved.

-- Example Unleash client using unleash-client-haskell-core, async and MVars.
-- Spawns a state poller thread that updates the feature toggles, a metrics
-- sender thread, and an application that continuously reads a feature toggle.
-- The application will block until the first feature toggle set is received.

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Concurrent.MVar
import Control.Monad (forever, unless, void)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Void (Void)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, Scheme (Http), mkClientEnv)
import Unleash
import Unleash.HttpClient (getAllClientFeatures, register, sendMetrics)

host :: String
host = "your-unleash-server"

port :: Int
port = 80

featureToggle :: Text
featureToggle = "your-feature-toggle"

main :: IO ()
main = do
    config <- makeConfig
    registerApp config
    let threads = ($ config) <$> [pollState, pushMetrics, application]
    runConcurrently $ traverse_ Concurrently threads

application :: Config -> IO Void
application config = do
    forever do
        enabled <- isEnabled config featureToggle
        putStrLn $ T.unpack featureToggle <> " is " <> (if enabled then "enabled" else "disabled")
        threadDelay $ 2 * 1000 * 1000

-- This function will block until the first feature toggle set has been received from the Unleash server.
isEnabled :: Config -> Text -> IO Bool
isEnabled config featureToggle = do
    state <- readMVar (state config)
    enabled <- featureIsEnabled state featureToggle emptyContext
    modifyMVar_ (metrics config) (\info -> pure $ (featureToggle, enabled) : info)
    pure enabled

registerApp :: Config -> IO ()
registerApp config = do
    now <- getCurrentTime
    let payload :: RegisterPayload
        payload =
            RegisterPayload
                { appName = "example-client",
                  instanceId = instanceName config,
                  started = now,
                  intervalSeconds = metricsPushIntervalInSeconds config
                }
    response <- register (httpClientEnvironment config) Nothing payload
    case response of
        Left error -> putStrLn $ "Could not register application (" <> show error <> ")"
        Right _ -> putStrLn "Application registered"

pollState :: Config -> IO Void
pollState config = do
    forever do
        response <- getAllClientFeatures (httpClientEnvironment config) Nothing
        case response of
            Left newState -> putStrLn $ "Could not get state (" <> show newState <> ")"
            Right newState -> do
                putStrLn "State received"
                updateState (state config) newState
        threadDelay $ (statePollIntervalInSeconds config) * 1000 * 1000
    where
        updateState state value = do
            isUpdated <- tryPutMVar state value
            unless isUpdated . void $ swapMVar state value

pushMetrics :: Config -> IO Void
pushMetrics config = do
    forever do
        threadDelay $ (metricsPushIntervalInSeconds config) * 1000 * 1000
        now <- getCurrentTime
        lastBucketStart <- swapMVar (metricsBucketStart config) now
        bucket <- swapMVar (metrics config) mempty
        let metricsPayload =
                MetricsPayload
                    { appName = "example-client",
                      instanceId = instanceName config,
                      start = lastBucketStart,
                      stop = now,
                      toggles = bucket
                    }
        response <- sendMetrics (httpClientEnvironment config) Nothing metricsPayload
        case response of
            Left error -> putStrLn $ "Could not send metrics (" <> show error <> ")"
            Right _ -> putStrLn "Metrics sent"

makeConfig :: IO Config
makeConfig = do
    state <- newEmptyMVar
    metrics <- newMVar mempty
    now <- getCurrentTime
    metricsBucketStart <- newMVar now
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (BaseUrl Http host port mempty)
    pure $
        Config
            { instanceName = "localhost",
              state = state,
              statePollIntervalInSeconds = 4,
              metrics = metrics,
              metricsBucketStart = metricsBucketStart,
              metricsPushIntervalInSeconds = 8,
              httpClientEnvironment = clientEnv
            }

data Config = Config
    { instanceName :: Text,
      state :: MVar Unleash.Features,
      statePollIntervalInSeconds :: Int,
      metrics :: MVar [(Text, Bool)],
      metricsBucketStart :: MVar UTCTime,
      metricsPushIntervalInSeconds :: Int,
      httpClientEnvironment :: ClientEnv
    }
