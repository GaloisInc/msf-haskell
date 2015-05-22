-- |Bindings to the MSF core functionality where we can modify
-- attributes of the currently executing server.
module MSF.Core
  ( module Types.Core
  , core_add_module_path
  , core_module_stats
  , core_reload_modules
  , core_setg
  , core_unsetg
  , core_thread_list
  , core_thread_kill
  , core_version
  , core_stop
  , core_save
  ) where

import MSF.Monad
import Types.Core
import qualified RPC.Core as RPC

-- | Add new local directory as module path.
core_add_module_path :: (SilentCxt s) => FilePath -> MSF s ModuleStats
core_add_module_path path = prim $ \ addr auth -> do
  RPC.core_add_module_path addr auth path

-- | Get the number of modules loaded, broken down by type.
core_module_stats :: (SilentCxt s) => MSF s ModuleStats
core_module_stats = prim $ \ addr auth -> do
  RPC.core_module_stats addr auth

-- | Dump and reload all modules from all configured module paths.
core_reload_modules :: (SilentCxt s) => MSF s ModuleStats
core_reload_modules = prim $ \ addr auth -> do
  RPC.core_reload_modules addr auth

-- | Set a global datastore key/value.
core_setg :: (SilentCxt s) => String -> String -> MSF s ()
core_setg key value = prim $ \ addr auth -> do
  RPC.core_setg addr auth key value

-- | Unset a global datastore key.
core_unsetg :: (SilentCxt s) => String -> MSF s ()
core_unsetg key = prim $ \ addr auth -> do
  RPC.core_unsetg addr auth key

-- | Get a list of the statuses of all background threads, along with ID numbers
core_thread_list :: (SilentCxt s) => MSF s ThreadMap
core_thread_list = prim $ \ addr auth -> do
  RPC.core_thread_list addr auth

-- | Kill a background thread. ThreadName should match entry from core_thread_list
core_thread_kill :: (QuietCxt s) => ThreadName -> MSF s ()
core_thread_kill thId = prim $ \ addr auth -> do
  RPC.core_thread_kill addr auth thId

-- | Get basic version information about the running framework instance, Ruby interpreter, and RPC protocol.
core_version :: (SilentCxt s) => MSF s Version
core_version = prim $ \ addr auth -> do
  RPC.core_version addr auth

-- | Immediately shutdown the Metasploit server.
core_stop :: (QuietCxt s) => MSF s Result
core_stop = prim $ \ addr auth -> do
  RPC.core_stop addr auth

-- | Stash the global datastore of the framework instance to the server's disk.
core_save :: (SilentCxt s) => MSF s ()
core_save = prim $ \ addr auth -> do
  RPC.core_save addr auth

