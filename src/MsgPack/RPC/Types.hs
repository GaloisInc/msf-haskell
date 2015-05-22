module MsgPack.RPC.Types where

import MsgPack

import Control.Applicative ((<$>),(<*>))
import Data.Int (Int32)


type MsgId = Int32

type Method = String


-- Message Tags ----------------------------------------------------------------

data MessageType
  = MsgRequest
  | MsgResponse
  | MsgNotify
    deriving (Show)

instance ToObject MessageType where
  toObject msg = case msg of
    MsgRequest  -> toObject (0 :: Int)
    MsgResponse -> toObject (1 :: Int)
    MsgNotify   -> toObject (2 :: Int)

instance FromObject MessageType where
  fromObject obj = do
    n <- destObjectInt obj
    case n of
      0 -> return MsgRequest
      1 -> return MsgResponse
      2 -> return MsgNotify
      _ -> fail "Invalid message type"


-- Requests --------------------------------------------------------------------

data Request = Request
  { requestId     :: MsgId
  , requestMethod :: Method
  , requestParams :: [Object]
  } deriving (Show)

instance ToObject Request where
  toObject req = toObject
    [ toObject MsgRequest
    , toObject (requestId req)
    , toObject (requestMethod req)
    , toObject (requestParams req)
    ]

instance FromObject Request where
  fromObject obj = do
    [ty,i,m,ps] <- fromObject obj
    MsgRequest  <- fromObject ty
    Request <$> fromObject i
            <*> fromObject m
            <*> fromObject ps


-- Responses -------------------------------------------------------------------

data Response = Response
  { responseId     :: MsgId
  , responseResult :: Either Object [Object]
  } deriving (Show)

instance ToObject Response where
  toObject resp = toObject
     $ toObject MsgResponse
     : toObject (responseId resp)
     : case responseResult resp of
         Left err  -> [ err,         toObject ()  ]
         Right res -> [ toObject (), toObject res ]

instance FromObject Response where
  fromObject obj = do
    [ty,i,err,r] <- fromObject obj
    MsgResponse  <- fromObject ty
    Response <$> fromObject i
             <*> case destObjectNil err of
                   Just () -> Right `fmap` fromObject r
                   Nothing -> return (Left err)


-- Notifications ---------------------------------------------------------------

data Notify = Notify
  { notifyMethod :: Method
  , notifyParams :: [Object]
  } deriving (Show)

instance ToObject Notify where
  toObject n = toObject
    [ toObject MsgNotify
    , toObject (notifyMethod n)
    , toObject (notifyParams n)
    ]

instance FromObject Notify where
  fromObject obj = do
    [ty,m,ps] <- fromObject obj
    MsgNotify <- fromObject ty
    Notify <$> fromObject m
           <*> fromObject ps
