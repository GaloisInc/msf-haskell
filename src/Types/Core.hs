module Types.Core
  ( module RPC.Util
  , ThreadId (..)
  , ModuleStats (..)
  , Version (..)
  , Thread (..)
  , ThreadName
  , ThreadMap
  ) where

import RPC.Util

import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map

type ThreadName = String

--------------------------------------------------------------------------------

newtype ThreadId = ThreadId
  { threadId :: Int
  } deriving (Show,Eq,Ord)

instance ToObject ThreadId where
  toObject (ThreadId i) = toObject i

instance FromObject ThreadId where
  fromObject obj = ThreadId <$> fromObject obj

--------------------------------------------------------------------------------

data Version = Version
  { version :: String
  , versionRuby :: String
  , versionApi :: String
  } deriving (Show)

instance FromObject Version where
  fromObject obj = Version <$>
    lookupField "version" obj <*>
    lookupField "ruby" obj <*>
    lookupField "api" obj

--------------------------------------------------------------------------------

data ModuleStats = ModuleStats
  { statExploits  :: Int
  , statAuxiliary :: Int
  , statPost      :: Int
  , statEncoders  :: Int
  , statNops      :: Int
  , statPayloads  :: Int
  } deriving (Show,Eq,Ord)

instance FromObject ModuleStats where
  fromObject obj = ModuleStats <$>
    lookupField "exploits"  obj <*>
    lookupField "auxiliary" obj <*>
    lookupField "post"      obj <*>
    lookupField "encoders"  obj <*>
    lookupField "nops"      obj <*>
    lookupField "payload"   obj

--------------------------------------------------------------------------------

data Thread = Thread
  { threadStatus   :: String
  , threadCritical :: Bool
  , threadName     :: ThreadName
  , threadStarted  :: String
  } deriving (Show,Eq,Ord)

instance FromObject Thread where
  fromObject obj = do
    dict <- fromObject obj
    Thread <$> (fromObject =<< Map.lookup "status"   dict)
           <*> (fromObject =<< Map.lookup "critical" dict)
           <*> (fromObject =<< Map.lookup "name"     dict)
           <*> (fromObject =<< Map.lookup "started"  dict)

--------------------------------------------------------------------------------

type ThreadMap = Map.Map ThreadId Thread

