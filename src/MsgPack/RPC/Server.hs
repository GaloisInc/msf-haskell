module MsgPack.RPC.Server where

import MsgPack (pack,fromObject,unpack,toObject)
import MsgPack.RPC.Types

import Data.String (fromString)
import Network.HTTP.Server (Handler,err_response,respond,StatusCode(..))
import qualified Network.HTTP as HTTP
import qualified Data.ByteString as S


-- | Just handle all top-level requests as RPC requests.
rpcHandler :: (Request -> IO Response) -> Handler S.ByteString
rpcHandler handler _addr _url rq = case unpack (HTTP.rqBody rq) of

  Right obj -> case fromObject obj of
    Just req -> buildResponse `fmap` handler req
    Nothing  -> return (decodeError "Failed to decode a request")

  Left err  -> return (decodeError err)

buildResponse :: Response -> HTTP.Response S.ByteString
buildResponse resp = (respond OK :: HTTP.Response S.ByteString)
  { HTTP.rspBody = pack (toObject resp)
  }

decodeError :: String -> HTTP.Response S.ByteString
decodeError msg = (err_response BadRequest :: HTTP.Response S.ByteString)
  { HTTP.rspBody = fromString msg
  }
