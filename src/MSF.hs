-- |The Core Metasploit Framework (MSF) types are exported through
-- this module. The functionality in this module calls through to the
-- lower-level 'RPC' modules, wrapping them in context that provides
-- the logged-in status, a console to interact with, and an
-- event-handling framework.

module MSF (
    -- * MSF Monad
    MSF()
  , Silent(), SilentCxt()
  , Loud(),  LoudCxt(), loud
  , Quiet(), QuietCxt()
  --, Volume(..), loud
  --, SilentCxt()
  --, QuietCxt()
  --, LoudCxt()
  , io
  , spawn

    -- * Remote Hosts
    -- ** Individual Hosts
  , Host(..)
  , Attackable(), AttackCxt()
  , Scannable(), ScanCxt()
  , attackableHost

    -- ** Targets
  , Target, (&), cidr, to, single

    -- ** Connections
  , Con(..)
  , Server()

    -- * Authentication Options
  , login

    -- * Event Management
  , HandlerRef
  , HostInfo(..)
  , onHost, ignoreHost
  , onService, onHostService, ignoreService
  , onLoot, ignoreLoot
  , onCred, ignoreCred
  , onSession, ignoreSession

    -- * Scanning Operations
  , module MSF.Scan
  ) where

import MSF.Event
import MSF.Host
import MSF.Monad
import MSF.Scan
import MSF.Concurrent

import RPC.DB
