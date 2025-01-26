{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.List ((!?))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import System.IO
import System.Random
import System.Random.Shuffle
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 (Html, ToMarkup, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data WordOfTheDay = WordOfTheDay {word, translation :: Text}
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WordOfTheDay

instance FromJSON WordOfTheDay

instance ToMarkup WordOfTheDay where
  toMarkup WordOfTheDay {..} = H.html $ do
    H.head $ do
      H.title "Woord van de dag"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/public/stylesheet.css"
    H.body $
      H.div ! A.class_ "word-container" $
        H.p $
          H.toMarkup (T.toTitle word)

wodTemplate :: WordOfTheDay -> Html
wodTemplate = undefined

exampleWord :: WordOfTheDay
exampleWord = WordOfTheDay "Hij" "He"

data Status = OK
  deriving (Show, Read, Generic)

instance FromJSON Status

instance ToJSON Status

type HealthApi = "health" :> Get '[JSON] Status

type WodApi = Get '[HTML, JSON] WordOfTheDay

type PublicApi = "public" :> Raw

type Api = HealthApi :<|> WodApi :<|> PublicApi

api :: Proxy Api
api = Proxy @Api

serveHealth :: Server HealthApi
serveHealth = return OK

serveWod :: [WordOfTheDay] -> Server WodApi
serveWod ws = do
  let start = fromGregorian 2025 1 1
  today <- liftIO $ utctDay <$> getCurrentTime
  let diff = diffDays today start
  case ws !? (fromInteger diff) of
    Just word -> return word
    Nothing -> return exampleWord

serveApi :: [WordOfTheDay] -> Server Api
serveApi words = serveHealth :<|> serveWod words :<|> serveDirectoryWebApp "./public"

main :: IO ()
main = do
  Just ws <- decode <$> BS.readFile "words.json"
  let seed = mkStdGen 1
  let shuffledWs = shuffle' ws (length ws) seed
  putStrLn "Serving \"Woord van den dag\" on port 80"
  hFlush stdout
  run 80 $ serve api (serveApi shuffledWs)
