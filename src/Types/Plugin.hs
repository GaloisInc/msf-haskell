module Types.Plugin
  ( module RPC.Util
  , Plugins
  , PluginName (..)
  , PluginOptions (..)
  ) where

import RPC.Util

import Control.Applicative ((<$>))
import qualified Data.Map as Map

--------------------------------------------------------------------------------

newtype PluginName = PluginName
  { pluginName :: String
  } deriving (Show,Eq,Ord)

instance ToObject PluginName where
  toObject (PluginName n) = toObject n

instance FromObject PluginName where
  fromObject obj = PluginName <$> fromObject obj

--------------------------------------------------------------------------------

newtype PluginOptions = PluginOptions
  { pluginOptions :: Map.Map String String
  } deriving (Show,Eq,Ord)

instance ToObject PluginOptions where
  toObject (PluginOptions m) = toObject m

instance FromObject PluginOptions where
  fromObject obj = PluginOptions <$> fromObject obj

--------------------------------------------------------------------------------

type Plugins = [PluginName]

