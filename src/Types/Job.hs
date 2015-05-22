module Types.Job
  ( module RPC.Util
  , JobMap
  , JobName (..)
  , JobURIPath (..)
  , JobId (..)
  , JobInfo (..)
  ) where

import RPC.Util

import Control.Applicative ((<$>),(<*>),(<|>))
import qualified Data.Map as Map

--------------------------------------------------------------------------------

newtype JobName = JobName
  { getJobName :: String
  } deriving (Show,Eq,Ord)

instance FromObject JobName where
  fromObject obj = JobName <$> fromObject obj

--------------------------------------------------------------------------------

newtype JobURIPath = JobURIPath
  { getJobURIPath :: String
  } deriving (Show,Eq,Ord)

instance FromObject JobURIPath where
  fromObject obj = JobURIPath <$> fromObject obj

--------------------------------------------------------------------------------

newtype JobId = JobId
  { getJobId :: String
  } deriving (Show,Eq,Ord)

instance ToObject JobId where
  toObject (JobId i) = toObject i

instance FromObject JobId where
  fromObject obj = asInt <|> asString
    where
    asInt    = JobId . show <$> (fromObject obj :: Maybe Int)
    asString = JobId        <$>  fromObject obj

--------------------------------------------------------------------------------

data JobInfo = JobInfo
  { jobId        :: JobId
  , jobName      :: JobName
  , jobStartTime :: Int
  , jobUriPath   :: Optional JobURIPath
  , jobDataStore :: Optional String
  } deriving (Show,Eq,Ord)

instance FromObject JobInfo where
  fromObject obj = JobInfo <$>
    lookupField "jid"        obj <*>
    lookupField "name"       obj <*>
    lookupField "start_time" obj <*>
    lookupField "uripath"    obj <*>
    lookupField "datastore"  obj

--------------------------------------------------------------------------------

type JobMap = Map.Map JobId String

