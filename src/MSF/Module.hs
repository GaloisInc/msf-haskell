-- |Interact with exploits, payloads, encoders, etc. Functionality for
-- executing a module with a particular payload is here.
module MSF.Module
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
  , module_execute_payload
  , module_execute
  ) where

import MSF.Monad
import Types.Module
import qualified RPC.Module as RPC

-- | List modules installed on server. Silent operation.
module_exploits :: (SilentCxt s) => MSF s Modules
module_exploits = prim RPC.module_exploits

-- | List aux modules installed on server. Silent operation.
module_auxiliary :: (SilentCxt s) => MSF s Modules
module_auxiliary = prim RPC.module_auxiliary

-- | List of post modules. Silent operation.
module_post :: (SilentCxt s) => MSF s Modules
module_post = prim RPC.module_post

-- |List of payload modules. Silent operation.
module_payloads :: (SilentCxt s) => MSF s Modules
module_payloads = prim RPC.module_payloads

-- |List of encoder modules. Silent operation.
module_encoders :: (SilentCxt s) => MSF s Modules
module_encoders = prim RPC.module_encoders

-- |List of nop modules. Silent operation.
module_nops :: (SilentCxt s) => MSF s Modules
module_nops = prim RPC.module_nops

-- | XXX results not parsed. Are only the keys listed in documentation possible here? Silent operation.
module_info :: (SilentCxt s) => ModuleType -> ModuleName -> MSF s ModuleInfo
module_info modTyp modNm = prim $ \ addr auth ->
  RPC.module_info addr auth modTyp modNm

-- XXX results are parsed, but parsing needs testing. Silent operation.
module_options :: (SilentCxt s) => ModuleType -> ModuleName -> MSF s ModuleOptions
module_options modTyp modNm = prim $ \ addr auth ->
  RPC.module_options addr auth modTyp modNm

-- |Payloads that are compatible with the given exploit. Silent operation.
module_compatible_payloads :: (SilentCxt s) => ModuleName -> MSF s Payloads
module_compatible_payloads modNm = prim $ \ addr auth ->
  RPC.module_compatible_payloads addr auth modNm

-- |Probably a silent operation, but not clear how it determines compatibility.
module_target_compatible_payloads :: (SilentCxt s) => ModuleName -> Int -> MSF s Payloads
module_target_compatible_payloads modNm targetIndex = prim $ \ addr auth ->
  RPC.module_target_compatible_payloads addr auth modNm targetIndex

-- |Probably silent, but not clear how it determines compatibility.
module_compatible_sessions :: (SilentCxt s) => ModuleName -> MSF s Sessions
module_compatible_sessions modNm = prim $ \ addr auth ->
  RPC.module_compatible_sessions addr auth modNm

-- |Silent operation.
module_encode :: (SilentCxt s) => Payload -> ModuleName -> EncodeOptions -> MSF s ModuleEncode
module_encode pl encoderModule opts = prim $ \ addr auth ->
  RPC.module_encode addr auth pl encoderModule opts

-- |Execute a payload type module. This is silent since it just returns a payload from MSF. Return value should be ExecPayload.
module_execute_payload :: (SilentCxt s) => Payload -> ExecuteOptions -> MSF s ExecResult
module_execute_payload (Payload pl) opts =
  loud $ module_execute PayloadModuleType (ModuleName pl) opts

-- |Most generic execution of module. Potentially silent and potentially loud.
module_execute :: (LoudCxt s) => ModuleType -> ModuleName -> ExecuteOptions -> MSF s ExecResult
module_execute modTyp modNm opts = prim $ \ addr auth ->
  RPC.module_execute addr auth modTyp modNm opts

