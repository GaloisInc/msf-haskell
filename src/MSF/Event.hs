-- |Event handling framework. Supports callbacks for discovery of
-- hosts, creation of sessions, discovery of credentials, etc.

module MSF.Event (
    module MSF.Event
  , HandlerRef()
  ) where

import MSF.Event.Handler
import MSF.Event.Prim
import MSF.Host
import MSF.Monad
import RPC.DB
import RPC.Session
import RPC.Job

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.Map as Map


mkHandler :: (HandlerRef -> a -> MSF s ())
          -> MSF s (HandlerRef -> a -> IO ())
mkHandler h = do
  run <- embed
  return (\ ref a -> run (h ref a))


-- | Listen for new hosts.
onHost :: (HandlerRef -> HostInfo -> MSF s ()) -> MSF s HandlerRef
onHost handler = do
  k    <- mkHandler handler
  cont <- controlHandle
  io (addHostHandler cont k)

-- | Disconnect a host handler.
ignoreHost :: HandlerRef -> MSF s ()
ignoreHost ref = do
  cont <- controlHandle
  io (remHostHandler cont ref)

onHostService :: Host t -> (HandlerRef -> Service -> MSF s ())
              -> MSF s HandlerRef
onHostService host k =
  onService $ \ ref svc ->
    when (serviceHost svc == castHost host) (k ref svc)

-- | Listen for new services.
onService :: (HandlerRef -> Service -> MSF s ()) -> MSF s HandlerRef
onService handler = do
  k    <- mkHandler handler
  cont <- controlHandle
  io (addServiceHandler cont k)

-- | Disconnect a service handler.
ignoreService :: HandlerRef -> MSF s ()
ignoreService ref = do
  cont <- controlHandle
  io (remServiceHandler cont ref)

-- | Listen for new creds.
onCred :: (HandlerRef -> Cred -> MSF s ()) -> MSF s HandlerRef
onCred handler = do
  k    <- mkHandler handler
  cont <- controlHandle
  io (addCredHandler cont k)

-- | Disconnect a cred handler.
ignoreCred :: HandlerRef -> MSF s ()
ignoreCred ref = do
  cont <- controlHandle
  io (remCredHandler cont ref)

-- | Listen for new loot.
onLoot :: (HandlerRef -> Loot -> MSF s ()) -> MSF s HandlerRef
onLoot handler = do
  k    <- mkHandler handler
  cont <- controlHandle
  io (addLootHandler cont k)

-- | Disconnect a loot handler.
ignoreLoot :: HandlerRef -> MSF s ()
ignoreLoot ref = do
  cont <- controlHandle
  io (remLootHandler cont ref)

-- | Listen for new session.
onSession :: (HandlerRef -> (SessionId,Session) -> MSF s ()) -> MSF s HandlerRef
onSession handler = do
  k    <- mkHandler handler
  cont <- controlHandle
  io (addSessionHandler cont k)

-- | Disconnect a service handler.
ignoreSession :: HandlerRef -> MSF s ()
ignoreSession ref = do
  cont <- controlHandle
  io (remSessionHandler cont ref)

-- | Wait until a job is done
waitJob :: JobId -> MSF v ()
waitJob jid = loop
  where
  loop = do
    jm <- prim job_list
    if Map.member jid jm
       then do
         io $ do
           threadDelay 500000
         loop
       else return ()
