-- |A simple msgpack remote procedure call (rpc) system for
-- interacting with the MSF server.
module RPC.SimpleRPC (request) where

import MsgPack

import Data.Char
import Data.Maybe
import Network.HTTP (simpleHTTP)
import Network.HTTP.Base (Request (..), Response (..), RequestMethod (..))
import Network.HTTP.Headers (Header (..), HeaderName (..))
import Network.Stream (Result)
import Network.URI (URI(..),URIAuth(..))

import qualified Data.ByteString as BS

{-| request addr obj establishes a connection to the metasploit server
    and then passes obj in a POST request, returning the Object formatted 
    response. -} 
request :: URI -> Object -> IO Object
request uri obj =
  simpleHTTP req >>= handleTransportErrors >>= handleApplicationErrors
  where
  req = Request {
    rqURI    = uri,
    rqMethod = POST,
    rqHeaders =
      [ Header HdrUserAgent "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)"
      , Header HdrContentType "binary/message-pack"
      , Header HdrContentLength (show . BS.length $ body)
      ] ++ hostHdr,
    rqBody = body
    }
  body = pack obj
  hostHdr = fromMaybe [] $ do
    auth <- uriAuthority uri
    return [ Header HdrHost (uriRegName auth ++ uriPort auth) ]

handleTransportErrors :: Result (Response a) -> IO (Response a)
handleTransportErrors (Right a) = return a
handleTransportErrors (Left e)  = error (show e)

handleApplicationErrors :: Response BS.ByteString -> IO Object
handleApplicationErrors response =
  case rspCode response of
    (2,_,_) -> case unpack (rspBody response) of
      Right obj -> return obj
      Left err  -> fail ("Unable to unpack response: " ++ err)
    (x,y,z) -> error (map intToDigit [x,y,z] ++ ' ' : (rspReason response))
