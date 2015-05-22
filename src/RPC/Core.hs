-- |Bindings to the MSF core functionality where we can modify
-- attributes of the currently executing server.
module RPC.Core
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

import MSF.Host (Con,Server)
import Types.Core

-- | Silent operation.
core_add_module_path :: Con Server -> Token -> FilePath -> IO ModuleStats
core_add_module_path addr auth path = send_request "core.add_module_path" addr
  [ toObject auth
  , toObject path
  ]

-- | Silent operation.
core_module_stats :: Con Server -> Token -> IO ModuleStats
core_module_stats addr auth = send_request "core.module_stats" addr
  [ toObject auth
  ]

-- | Silent operation.
core_reload_modules :: Con Server -> Token -> IO ModuleStats
core_reload_modules addr auth = send_request "core.reload_modules" addr
  [ toObject auth
  ]

-- | Silent operation. Local to the server, but could have complex side effects.
core_setg :: Con Server -> Token -> String -> String -> IO ()
core_setg addr auth key value = success "core.setg" =<< send_request "core.setg" addr
  [ toObject auth
  , toObject key
  , toObject value
  ]

-- | Silent operation.
core_unsetg :: Con Server -> Token -> String -> IO ()
core_unsetg addr auth key = success "core.unsetg" =<< send_request "core.unsetg" addr
  [ toObject auth
  , toObject key
  ]

-- | Silent operation.
core_thread_list :: Con Server -> Token -> IO ThreadMap
core_thread_list addr auth = send_request "core.thread_list" addr
  [ toObject auth
  ]

-- | Quiet action. Potentially not silent since it could stop active jobs?
core_thread_kill :: Con Server -> Token -> ThreadName -> IO ()
core_thread_kill addr auth threadID = success "core.thread_kill" =<< send_request "core.thread_kill" addr
  [ toObject auth
  , toObject threadID
  ]

-- | Silent operation.
core_version :: Con Server -> Token -> IO Version
core_version addr auth = send_request "core.version" addr
  [ toObject auth
  ]

-- | Stops metasploit server altogether; only use in extreme cases. Quiet action but could stop running sessions, jobs, etc.
core_stop :: Con Server -> Token -> IO Result
core_stop addr auth = send_request "core.stop" addr
  [ toObject auth
  ]

-- | Silent operation.
core_save :: Con Server -> Token -> IO ()
core_save addr auth = success "core.save" =<< send_request "core.save" addr
  [ toObject auth
  ]

