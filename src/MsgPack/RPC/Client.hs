{-# LANGUAGE FlexibleInstances #-}

module MsgPack.RPC.Client where

import MsgPack
import MsgPack.RPC.Types
import RPC.SimpleRPC

import Network.URI (URI)


-- | Invoke a method on the server.
call :: Call req => URI -> MsgId -> Method -> req
call  = call' []


class Call req where
  call' :: [Object] -> URI -> MsgId -> Method -> req

instance Call (IO Response) where
  call' params url i m = do
    obj <- request url $ toObject Request
      { requestId     = i
      , requestMethod = m
      , requestParams = reverse params
      }
    case fromObject obj of
      Just resp -> return resp
      Nothing   -> fail "Unable to decode response"

instance (ToObject a, Call req) => Call (a -> req) where
  call' params url i m a = call' (toObject a : params) url i m
