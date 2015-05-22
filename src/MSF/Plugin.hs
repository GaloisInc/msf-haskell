-- |Load and unload MSF plugins.
module MSF.Plugin
  ( module Types.Plugin
  , plugin_load
  , plugin_unload
  , plugin_loaded
  ) where

import MSF.Monad
import Types.Plugin
import qualified RPC.Plugin as RPC

-- | Silent operation.
plugin_load :: (SilentCxt s) => PluginName -> PluginOptions -> MSF s Result
plugin_load name opts = prim $ \ addr auth -> do
  RPC.plugin_load addr auth name opts

-- | Unload a plugin, but names are not always compatible with plugin_load. Silent operation.
plugin_unload :: (SilentCxt s) => PluginName -> MSF s Result
plugin_unload name = prim $ \ addr auth -> do
  RPC.plugin_unload addr auth name

-- | Enumerate loaded plugins. Silent operation.
plugin_loaded :: (SilentCxt s) => MSF s Plugins
plugin_loaded = prim $ \ addr auth -> do
  RPC.plugin_loaded addr auth

