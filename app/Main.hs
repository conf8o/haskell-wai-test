{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTypes
import Network.HTTP.Types (StdMethod (GET))
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)

type Path = [T.Text]
type RoutingInfo = (HTypes.StdMethod, Path)

toRoutingInfo :: Wai.Request -> Either B.ByteString RoutingInfo
toRoutingInfo req =
    fmap (, Wai.pathInfo req)
    $ HTypes.parseMethod . Wai.requestMethod
    $ req

router :: Wai.Application
router req =
    case toRoutingInfo req of
        Right routingInfo -> routing routingInfo req
        _ -> notFound req

routing :: RoutingInfo -> Wai.Application
routing (GET, [""]) = getIndex
routing (GET, ["articles"]) = getArticles
routing _ = notFound

main :: IO ()
main = Warp.run 8000 router

getIndex :: Wai.Application
getIndex _ send =
    send $ Wai.responseBuilder HTypes.status200 [] "OK"

getArticles :: Wai.Application
getArticles req send = 
    send $ Wai.responseBuilder HTypes.status201 [] $ byteString (Wai.rawPathInfo req)

postArticles :: Wai.Application
postArticles req send =
    send $ Wai.responseBuilder HTypes.status201 [] ""

notFound :: Wai.Application
notFound _ send =
    send $ Wai.responseBuilder HTypes.status404 [] "not found"