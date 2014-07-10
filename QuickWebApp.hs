{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances         #-}
module QuickWebApp where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.ByteString  as BS
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Text  as TS
import           Web.Scotty

class ToLBS a where
    toLBS :: a -> BL.ByteString

instance ToLBS BL.ByteString where
    toLBS = id

instance ToLBS BS.ByteString where
    toLBS = BL.fromStrict

instance ToLBS TL.Text where
    toLBS = TL.encodeUtf8

instance ToLBS TS.Text where
    toLBS = TL.encodeUtf8 . TL.fromStrict

instance ToLBS String where
    toLBS = BC.pack

class FromLBS a where
    fromLBS :: BL.ByteString -> a

instance FromLBS BL.ByteString where
    fromLBS = id

instance FromLBS BS.ByteString where
    fromLBS = BL.toStrict

instance FromLBS TL.Text where
    fromLBS = TL.decodeUtf8

instance FromLBS TS.Text where
    fromLBS = TL.toStrict . TL.decodeUtf8

instance FromLBS String where
    fromLBS = BC.unpack

-- | 'interactWebOn 3000'
interactWeb :: (FromLBS a, ToLBS b) => (a -> b) -> IO ()
interactWeb = interactWebOn 3000

-- | Create an API with multiple endpoints:
--
-- - a 'POST' endpoint per transformer (at URI 'name')
-- - a 'GET' endpoint, which lists all the above endpoints
interactWebOn :: (FromLBS a, ToLBS b) => Int -> (a -> b) -> IO ()
interactWebOn port f = scotty port $ post "/" $ do
                c <- body
                setHeader "Content-Type" "text/plain"
                raw . toLBS . f . fromLBS $ c
