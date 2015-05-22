-- |Helpful utility functions.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module RPC.Util (
    -- * Message interface
    send_request
  , DecodeError(..)
  , decodeError
  , WriteFailed(..)
  , writeFailed
  , Token(..)

    -- * Encoding interface
  , ToObject(..)
  , FromObject(..)

    -- * Utilities
  , destObjectNil
  , destObjectBool
  , destObjectInt
  , destObjectFloat
  , destObjectDouble
  , destObjectArray
  , destObjectMap
  , destObjectRaw
  , lookupField
  , success
  , field
  , Result (..)
  , Optional (..)
  , optionalToObject
    -- * Re-exported
  , Object(..)
  , WriteCount
  , Tabs
  , Modules
  ) where

import MSF.Host (Server,Host(..),Con(..))

import RPC.SimpleRPC (request)
import MsgPack

import Text.Show.Pretty

import Control.Applicative ((<$>),(<*>),(<|>),pure)
import Control.Monad (guard)
import Data.Maybe
import Data.Typeable
import Network.URI (URI(..),URIAuth(..),nullURI)
import qualified Data.Map as Map
import qualified Control.Exception as X

-- Common Type Synonyms --------------------------------------------------------

type WriteCount = Int
type Tabs = [String]
type Modules = [String]

-- Request/Response ------------------------------------------------------------

data WriteFailed
  = WriteFailed String
  deriving (Show,Typeable,Eq)

instance X.Exception WriteFailed

writeFailed :: String -> IO a
writeFailed = X.throwIO . WriteFailed

data DecodeError
  = DecodeError String Object
  | ErrorMsg String Int String
  deriving (Show,Typeable,Eq)

instance X.Exception DecodeError

decodeError :: String -> Object -> IO a
decodeError src obj = error $ ppShow $
  fromMaybe
    (DecodeError src obj)
    (ErrorMsg <$> pure src <*>
     lookupField "error_code" obj <*>
     lookupField "error_message" obj)

addrToURI :: Con Server -> URI
addrToURI con = nullURI
  { uriScheme    = "http:"
  , uriAuthority = Just URIAuth
    { uriUserInfo = ""
    , uriRegName  = getHost (conHost con)
    , uriPort     = if null (conPort con)
                       then ""
                       else ':' : conPort con
    }
  , uriPath      = "/api/"
  }

instance (FromObject a) => FromObject (Either String a) where
  fromObject obj =
    (Right <$> fromObject obj) <|>
    (Left <$> lookupField "error_message" obj)

data Result
  = Success
  | Failure
  deriving Show

instance FromObject Result where
  fromObject obj = do
    m <- fromObject obj
    case Map.lookup "result" m of
      Just "failure" -> return Failure
      Just "success" -> return Success
      _              -> Nothing

instance (FromObject a) => FromObject (Maybe a) where
  fromObject obj = 
    (lookupField "result" obj >>= guard . (== "failure") >> return Nothing) <|>
    (Just <$> fromObject obj)

optionalToObject :: (ToObject a) => String -> Optional a -> Maybe (String,Object)
optionalToObject fld opt = case opt of
  Absent -> Nothing
  Present a -> Just (fld,toObject a)

newtype Token = Token String
    deriving (Show,Eq,Ord)

instance ToObject Token where
  toObject (Token tok) = toObject tok

instance FromObject Token where
  fromObject obj = Token <$> lookupField "token" obj

--send_request :: (ToObject req, FromObject resp) => String -> Con Server -> req -> IO resp
send_request :: (FromObject resp) => String -> Con Server -> [Object] -> IO resp
send_request src addr req = do
  obj <- request (addrToURI addr) (toObject (toObject src : req))
  case fromObject obj of
    Just resp -> return resp
    Nothing   -> decodeError src obj

field :: (FromObject a) => String -> String -> Object -> IO a
field src f obj = case lookupField f obj of
  Just a -> return a
  Nothing -> decodeError src obj

-- Simple Responses ------------------------------------------------------------

success :: String -> Object -> IO ()
success src obj = case mb of
  Just () -> return ()
  Nothing -> decodeError src obj
  where
  mb = do
    dict <- fromObject obj
    r    <- Map.lookup "result" dict
    r `asTypeOf` obj `seq` return ()

-- Object Manipulation ---------------------------------------------------------

data Optional a
  = Present a
  | Absent
  deriving (Eq,Show,Ord)

instance (FromObject a) => FromObject (Optional a) where
  fromObject obj = (Present <$> fromObject obj) <|> return Absent
  defaultVal = Just Absent

-- | Lookup and parse a single field in an object.
lookupField :: FromObject a => String -> Object -> Maybe a
lookupField f obj = do
  m <- fromObject obj
  case Map.lookup f m of
    Just a  -> return a
    Nothing -> defaultVal

