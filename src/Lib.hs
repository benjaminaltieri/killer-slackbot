{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( resume_prime_directive
    ) where

import System.IO
import System.Environment
import qualified Configuration.Dotenv as Dotenv
import Configuration.Dotenv.Types (defaultConfig)
import Network.Wreq
import qualified Data.Text as T
import Control.Lens ((&), (^.), (.~))
import qualified Data.ByteString.Lazy.Char8 as BSL

resume_prime_directive :: IO ()
resume_prime_directive = do
    -- don't buffer IO
    hSetBuffering stdout NoBuffering
    -- load our .env and get our slackbot token
    Dotenv.loadFile defaultConfig
    token <- T.pack <$> getEnv "SLACK_API_TOKEN"
    -- setup our API request
    let opts = defaults & param "token" .~ [token]
    r <- getWith opts "https://slack.com/api/rtm.connect"
    putStrLn $ BSL.unpack $ r ^. responseBody
