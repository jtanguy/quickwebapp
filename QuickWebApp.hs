{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-| A quick-and-dirty api generator, for any function `a -> b` which can be wrapped
    inside a function `ByteString -> ByteString`.
    It is inspired from the 'interact' function from 'Prelude'.
-}
module QuickWebApp where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text                  as TS
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Network.HTTP.Types.Status
import           Web.Scotty

-- | Represents types which can be converted to a Lazy 'ByteString'
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

-- | Represents types which can be converted from a Lazy 'ByteString'
-- This is intended for other String-like types
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

-- | Unprocessable entity error code (temporary fix for a missing status in
-- http-types)
err422 :: Status
err422 = mkStatus 422 "Unprocessable Entity"

-- | Equivalent to 'interactWebOn 3000'
interactWeb :: (FromLBS a, ToLBS b) => (a -> b) -> IO ()
interactWeb = interactWebOn 3000

-- | Equivalent to 'interactWebEitherOn 3000'
interactWebEither :: (FromLBS a, ToLBS b, ToLBS e) => (a -> Either e b) -> IO ()
interactWebEither = interactWebEitherOn 3000

-- | Create an API with a 'POST' endpoint
interactWebOn :: (FromLBS a, ToLBS b) => Int -> (a -> b) -> IO ()
interactWebOn port f = scotty port $ post "/" $ do
                c <- body
                setHeader "Content-Type" "text/plain"
                raw . toLBS . f . fromLBS $ c

-- | Create an API with a 'POST' endpoint
-- If the function fails and returns a 'Left' value, return a 422 response with
-- the error in the body
interactWebEitherOn :: (FromLBS a, ToLBS b, ToLBS e) => Int -> (a -> Either e b) -> IO ()
interactWebEitherOn port f = scotty port $ post "/" $ do
                c <- body
                setHeader "Content-Type" "text/plain"
                case f . fromLBS $ c of
                    Right res -> raw . toLBS $ res
                    Left err -> status err422 >> (raw . toLBS $ err)

