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
import Control.Lens ((&), (^?), (.~))
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson.Lens
import Wuss hiding (defaultConfig)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.URI
import Control.Monad (forever)
import Data.Aeson

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
    let murl = T.unpack <$> r ^? responseBody . key "url" . _String
        muri = parseURI =<< murl
        mhost = uriRegName <$> (uriAuthority =<< muri)
        mpath = uriPath <$> muri

    case (mhost, mpath) of
        (Just host, Just path) -> runSecureClient host 443 path ws
        otherwise -> fail "Could not obtain Slack RTM URL"

ws :: ClientApp ()
ws connection = do
    print "Connected!"
    forever $ do
        message <- receiveData connection
        case decode message of
          Nothing -> pure ()
          Just msg -> print $ content msg

data SlackMessage = SlackMessage
    { userId :: String
    , content :: String
    , channelId :: String
    } deriving (Show)

instance FromJSON SlackMessage where
    parseJSON = withObject "SlackMessage" $ \o -> SlackMessage
        <$> o .: "user"
        <*> o .: "text"
        <*> o .: "channel"

