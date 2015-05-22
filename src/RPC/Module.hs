-- |Interact with exploits, payloads, encoders, etc. Functionality for
-- executing a module with a particular payload is here.
module RPC.Module
  ( module Types.Module
  , module_exploits
  , module_auxiliary
  , module_post
  , module_payloads
  , module_encoders
  , module_nops
  , module_info
  , module_options
  , module_compatible_payloads
  , module_target_compatible_payloads
  , module_compatible_sessions
  , module_encode
  , module_execute
  ) where

import MSF.Host (Con,Server)
import Types.Module

-- | List modules installed on server. Silent operation.
module_exploits :: Con Server -> Token -> IO Modules
module_exploits addr auth = field "module.exploits" "modules" =<< send_request "module.exploits" addr
  [ toObject auth
  ]

-- | List aux modules installed on server. Silent operation.
module_auxiliary :: Con Server -> Token -> IO Modules
module_auxiliary addr auth = field "module.auxiliary" "modules" =<< send_request "module.auxiliary" addr
  [ toObject auth
  ]

-- | List of post modules. Silent operation.
module_post :: Con Server -> Token -> IO Modules
module_post addr auth = field "module.post" "modules" =<< send_request "modules.post" addr
  [ toObject auth
  ]

-- |List of payload modules. Silent operation.
module_payloads :: Con Server -> Token -> IO Modules
module_payloads addr auth = field "module.payloads" "modules" =<< send_request "module.payloads" addr
  [ toObject auth
  ]

-- |List of encoder modules. Silent operation.
module_encoders :: Con Server -> Token -> IO Modules
module_encoders addr auth = field "module.encoders" "modules" =<< send_request "module.encoders" addr
  [ toObject auth
  ]

-- |List of nop modules. Silent operation.
module_nops :: Con Server -> Token -> IO Modules
module_nops addr auth = field "module.nops" "modules" =<< send_request "module.nops" addr
  [ toObject auth
  ]

-- | XXX results not parsed. Are only the keys listed in documentation possible here? Silent operation.
module_info :: Con Server -> Token -> ModuleType -> ModuleName -> IO ModuleInfo
module_info addr auth modTyp modNm = send_request "module.info" addr
  [ toObject auth
  , toObject modTyp
  , toObject modNm
  ]

-- XXX results are parsed, but parsing needs testing. Silent operation.
module_options :: Con Server -> Token -> ModuleType -> ModuleName -> IO ModuleOptions
module_options addr auth modTyp modNm = send_request "module.options" addr
  [ toObject auth
  , toObject modTyp
  , toObject modNm
  ]

-- |Payloads that are compatible with the given exploit. Silent operation.
module_compatible_payloads :: Con Server -> Token -> ModuleName -> IO Payloads
module_compatible_payloads addr auth modNm = field "module.compatible_payloads" "payloads" =<< send_request "module.compatible_payloads" addr
  [ toObject auth
  , toObject modNm
  ]

-- |Probably a silent operation, but not clear how it determines compatibility.
module_target_compatible_payloads :: Con Server -> Token -> ModuleName -> Int -> IO Payloads
module_target_compatible_payloads addr auth modNm targetIndex = field "module.target_compatible_payloads" "payloads" =<< send_request "module.target_compatible_paylods" addr
  [ toObject auth
  , toObject modNm
  , toObject targetIndex
  ]

-- |Probably silent, but not clear how it determines compatibility.
module_compatible_sessions :: Con Server -> Token -> ModuleName -> IO Sessions
module_compatible_sessions addr auth modNm = field "module.compatible_sessions" "sessions" =<< send_request "module.compatible_sessions" addr
  [ toObject auth
  , toObject modNm
  ]

-- |Silent operation.
module_encode :: Con Server -> Token -> Payload -> ModuleName -> EncodeOptions -> IO ModuleEncode
module_encode addr auth pl encoderModule opts = field "module_encode" "encoded" =<< send_request "module.encode" addr
  [ toObject auth
  , toObject pl
  , toObject encoderModule
  , toObject opts
  ]

-- |Most generic execution of module. Potentially silent and potentially loud. TODO: Partial function!
module_execute :: Con Server -> Token -> ModuleType -> ModuleName -> ExecuteOptions -> IO ExecResult
module_execute _ _ (EncoderModuleType) _ _ = error "Can't use EncoderModuleType here"
module_execute _ _ (NopModuleType)     _ _ = error "Can't use NopModuleType here"
module_execute addr auth modTyp modNm opts = send_request "module.execute" addr
  [ toObject auth
  , toObject modTyp
  , toObject modNm
  , toObject opts
  ]

