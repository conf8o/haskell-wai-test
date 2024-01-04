{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTypes
import Network.HTTP.Types (StdMethod (GET))
import qualified Data.Text as T
import qualified Data.ByteString as B

main :: IO ()
main = Warp.run 8000 router

type Routinginfo = (HTypes.StdMethod, [T.Text])

parseRoutingInfo :: Wai.Request -> Either B.ByteString Routinginfo
parseRoutingInfo req =
    fmap (, Wai.pathInfo req)
    $ HTypes.parseMethod . Wai.requestMethod
    $ req

router :: Wai.Application
router req =
    case parseRoutingInfo req of
        Right routingInfo -> routing routingInfo req
        _ -> notFound req

routing :: Routinginfo -> Wai.Application
routing (GET, [""]) = getIndex
routing (GET, ["articles"]) = getArticles
routing _ = notFound

getIndex :: Wai.Application
getIndex _ send
    = send $ Wai.responseBuilder HTypes.status200 [] "OK"

getArticles :: Wai.Application
getArticles req send
    = send $ Wai.responseBuilder HTypes.status200 [] "hello wai"

notFound :: Wai.Application
notFound _ send
  = send $ Wai.responseBuilder HTypes.status404 [] "not found"