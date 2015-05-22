module Types.Session
  ( module RPC.Util
  , SessionId (..)
  , SessionMap (..)
  , Session (..)
  , ShellRead (..)
  , SessionWriteCount (..)
  , DirSep
  , ReadPointer
  , HostName
  , PortNumber
  ) where

import RPC.Util

import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map

--------------------------------------------------------------------------------

newtype SessionId = SessionId
  { getSessionId :: Int
  } deriving (Show,Ord,Eq)

instance ToObject SessionId where
  toObject = toObject . getSessionId

instance FromObject SessionId where
  fromObject obj = SessionId <$> fromObject obj

--------------------------------------------------------------------------------

newtype SessionMap = SessionMap
  { getSessionMap :: Map.Map SessionId Session
  } deriving (Show)

instance FromObject SessionMap where
  fromObject obj = SessionMap <$> fromObject obj

--------------------------------------------------------------------------------

data Session = Session
  { sessType
  , sessTunnelLocal
  , sessTunnelPeer
  , sessViaExploit
  , sessViaPayload
  , sessDesc
  , sessInfo
  , sessWorkspace
  , sessHost         :: HostName
  , sessPort         :: PortNumber
  , sessTargetHost
  , sessUsername
  , sessUUID
  , sessExploitUUID
  , sessRoutes       :: String
  } deriving (Show)

instance FromObject Session where
  fromObject obj = do
    m <- fromObject obj
    Session <$> (fromObject =<< Map.lookup "type"         m)
            <*> (fromObject =<< Map.lookup "tunnel_local" m)
            <*> (fromObject =<< Map.lookup "tunnel_peer"  m)
            <*> (fromObject =<< Map.lookup "via_exploit"  m)
            <*> (fromObject =<< Map.lookup "via_payload"  m)
            <*> (fromObject =<< Map.lookup "desc"         m)
            <*> (fromObject =<< Map.lookup "info"         m)
            <*> (fromObject =<< Map.lookup "workspace"    m)
            <*> (fromObject =<< Map.lookup "session_host" m)
            <*> (fromObject =<< Map.lookup "session_port" m)
            <*> (fromObject =<< Map.lookup "target_host"  m)
            <*> (fromObject =<< Map.lookup "username"     m)
            <*> (fromObject =<< Map.lookup "uuid"         m)
            <*> (fromObject =<< Map.lookup "exploit_uuid" m)
            <*> (fromObject =<< Map.lookup "routes"       m)

--------------------------------------------------------------------------------

data ShellRead = ShellRead
  { readSeq  :: Maybe ReadPointer
  , readData :: String
  } deriving (Eq,Show)

instance FromObject ShellRead where
  fromObject obj = ShellRead <$>
    lookupField "seq" obj <*>
    lookupField "data" obj

--------------------------------------------------------------------------------

newtype SessionWriteCount = SessionWriteCount
  { sessionWriteCount :: String
  } deriving (Eq,Show)

instance FromObject SessionWriteCount where
  fromObject obj = SessionWriteCount <$> lookupField "write_count" obj

--------------------------------------------------------------------------------

type ReadPointer = String
type HostName = String
type PortNumber = Int
type DirSep = String

