-- |Load and unload MSF plugins.
module RPC.Plugin
  ( module Types.Plugin
  , plugin_load
  , plugin_unload
  , plugin_loaded
  ) where

import MSF.Host (Con,Server)
import Types.Plugin

-- | Silent operation.
plugin_load :: Con Server -> Token -> PluginName -> PluginOptions -> IO Result
plugin_load addr auth pluginNm opts = send_request "plugin.load" addr
  [ toObject auth
  , toObject pluginNm
  , toObject opts
  ]

-- | Unload a plugin, but names are not always compatible with plugin_load. Silent operation.
plugin_unload :: Con Server -> Token -> PluginName -> IO Result
plugin_unload addr auth pluginNm = send_request "plugin.unload" addr
  [ toObject auth
  , toObject pluginNm
  ]

-- | Enumerate loaded plugins. Silent operation.
plugin_loaded :: Con Server -> Token -> IO Plugins
plugin_loaded addr auth = field "plugin.loaded" "plugins" =<< send_request "plugin.loaded" addr
  [ toObject auth
  ]

