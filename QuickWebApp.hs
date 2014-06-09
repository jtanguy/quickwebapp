{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- | Create a quick-and-dirty api
--
-- > interactWeb [ reverse <?> "reverse"
-- >             , id <?> "echo"
-- >             ]
module QuickWebApp where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson              (toJSON)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Network.Mime
import           Web.Scotty

-- | A 'Transformer' is a transformation from 'TL.Text' to 'TL.Text', with a name
-- and a 'MimeType'
data Transformer = Transformer { name   :: String
                               , mimeTo :: MimeType
                               , trans  :: TL.Text -> TL.Text
                               }

-- | Convert anything into a transformer with 'HasTransformer'
class HasTransformer m where
    getTransformer :: m -> Transformer

-- |
instance HasTransformer (TL.Text -> TL.Text) where
    getTransformer = Transformer "convert" "text/plain"

-- |
instance HasTransformer Transformer where
    getTransformer = id

-- | Give a transformer a name
(<?>) :: HasTransformer m => m -> String -> Transformer
t <?> newname = (getTransformer t) { name = newname }

-- | Give a transformer an output MimeType
(<::>) :: HasTransformer m => m -> MimeType -> Transformer
t <::> mime = (getTransformer t) { mimeTo = mime }

-- | 'interactWebOn 3000'
interactWeb :: forall m. HasTransformer m => [m] -> IO ()
interactWeb = interactWebOn 3000

-- | Create an API with multiple endpoints:
--
-- - a 'POST' endpoint per transformer (at URI 'name')
-- - a 'GET' endpoint, which lists all the above endpoints
interactWebOn :: forall m. HasTransformer m => Int -> [m] -> IO ()
interactWebOn port ts = scotty port $ do
        get "/" $ json . toJSON . map (name . getTransformer) $ ts
        forM_ ts $ \t -> post (literal . name . getTransformer $ t) $ do
                c <- param "contents" `rescue` const (BL.toStrict <$> body)
                setHeader "Content-Type" (TL.decodeUtf8 . BL.fromStrict . mimeTo . getTransformer $ t)
                raw . TL.encodeUtf8 . (trans . getTransformer $ t) . TL.decodeUtf8 . BL.fromStrict $ c
