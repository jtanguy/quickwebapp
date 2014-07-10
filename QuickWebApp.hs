{-# LANGUAGE OverloadedStrings         #-}
module QuickWebApp where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString  as BS
import           Web.Scotty

class ToLBS a where
    toLBS :: a -> BL.ByteString

instance ToLBS BL.ByteString where
    toLBS = id

instance ToLBS BS.ByteString where
    toLBS = BL.fromStrict

class FromLBS a where
    fromLBS :: BL.ByteString -> a

instance FromLBS BL.ByteString where
    fromLBS = id

instance FromLBS BS.ByteString where
    fromLBS = BL.toStrict

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
