{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-| A quick-and-dirty api generator, for any function `a -> b` which can be wrapped
    inside a function `ByteString -> ByteString`.
    It is inspired from the 'interact' function from 'Prelude'.
-}
module QuickWebApp (
    interactWeb
  ) where

import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Either.Combinators
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           System.Environment

import Lucid
import           Servant
import           Servant.HTML.Lucid

type API = Get '[HTML] Home
       :<|> ReqBody '[JSON, FormUrlEncoded] Input :> Post '[JSON] Output


data Home = Home
instance ToHtml Home where
  toHtml Home = doctypehtml_ $ do
    head_ $ do
      title_ "Api powered by QuickWebApp"
      meta_ [charset_ "utf-8"]
      link_ [rel_ "stylesheet", type_ "text/css",  href_ "http://groundfloor.neocities.org/default.css"]
    body_ $ do
      header_ $ do
        h1_ "interactWeb :: (a -> Either String b) -> IO ()"
        p_ $ do
          "Powered by "
          a_ [href_ "http://hackage.haskell.org/package/servant"] "servant"
          " and "
          a_ [href_ "http://hackage.haskell.org/package/lucid"] "lucid"
      div_ [ style_ "width: 80%; margin: auto;"] $ do
        div_ [style_ "display: flex; flex-direction: row; align-items: flex-stretch"] $ do
          section_ [style_ "flex: 1 1 50%;",  class_ "input"] $ do
            h2_ "Try out your function here"
            form_ [action_ "/" , method_ "POST"] $ do
              textarea_ [ name_ "input" ] ""
              input_ [type_ "submit" , value_ "Test"]
          section_ [style_ "flex: 1 1 50%", class_ "output"] $ do
            h2_ "Results here"
            pre_ $ samp_ [id_ "output"] ""
        h2_ "You can also curl or httpie"
        kbd_ "http :8080 input=\"<your input string>\""
        kbd_ "curl localhost:8080 -d input=\"<your input string>\""
        h2_ "Where to go from here ?"
        p_ $ do
          "TODO: This is where I show you the boilerplate to kickstart \
          \ your api, but I've not done that yet. "
          a_ [href_ "http://github.com/jtanguy/quickwebapp/issues/2"] "Corresponding github issue"
        term "script" [src_ "//code.jquery.com/jquery-1.11.3.min.js"] ""
      script_ "$(function(){ \
        \ $('form').submit(function(){ \
        \    $.post($(this).attr('action'), $(this).serialize(), function(json) { \
        \         $('#output').html(json.output);\
        \             }, 'json');\
        \                 return false;\
        \                   });\
        \                   });"
  toHtmlRaw = toHtml


newtype Input = Input { input :: Text } deriving (Show, Eq, Generic)

instance FromJSON Input
instance FromFormUrlEncoded Input where
  fromFormUrlEncoded = eitherDecode . encode . M.fromList

newtype Output = Output { output :: Text } deriving (Show, Eq, Generic)
instance ToJSON Output

interactWeb :: (FromText a, ToText b) => (a -> Either String b) -> IO ()
interactWeb f = do
    port <- maybe 8080 read <$> lookupEnv "PORT"
    run port (serve (Proxy :: Proxy API) (return Home :<|> handler))
  where
    handler = maybe (left $ err "Could not convert from text")
                    (hoistEither . mapBoth err (Output . toText) . f) . fromText . input

err :: String -> ServantErr
err e = ServantErr 422 "Unprocessable Entity" (BL8.pack (show e)) []

