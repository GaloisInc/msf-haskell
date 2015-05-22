module Types.Console
  ( module RPC.Util
  , Console (..)
  , ConsoleId (..)
  , ConsoleRead (..)
  ) where

import RPC.Util

import Control.Applicative ((<$>),(<*>))

--------------------------------------------------------------------------------

data ConsoleRead = ConsoleRead
  { consoleReadData :: String
  , consoleReadPrompt :: String
  , consoleReadBusy :: Bool
  } deriving (Show,Eq,Ord)

instance FromObject ConsoleRead where
  fromObject obj = ConsoleRead <$>
    lookupField "data" obj <*>
    lookupField "prompt" obj <*>
    lookupField "busy" obj

--------------------------------------------------------------------------------

newtype ConsoleId = ConsoleId
  { getConsoleId :: String
  } deriving (Show,Eq,Ord)

instance ToObject ConsoleId where
  toObject = toObject . getConsoleId

instance FromObject ConsoleId where
  fromObject obj = ConsoleId <$> fromObject obj

--------------------------------------------------------------------------------

data Console = Console
  { consoleId     :: ConsoleId
  , consolePrompt :: String
  , consoleBusy   :: Bool
  } deriving (Show,Eq,Ord)

instance FromObject Console where
  fromObject obj = Console <$>
    lookupField "id" obj <*>
    lookupField "prompt" obj <*>
    lookupField "busy" obj
