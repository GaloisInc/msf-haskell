-- |Low-level event-handling code.
module MSF.Event.Prim (

    -- * Polling Interface
    pollServices
  , ControlHandle

    -- * Control Messages
  , addHostHandler
  , remHostHandler
  , addServiceHandler
  , remServiceHandler
  , addCredHandler
  , remCredHandler
  , addLootHandler
  , remLootHandler
  , addSessionHandler
  , remSessionHandler
  ) where

import MSF.Event.Handler
import MSF.Host (Con,Server,Host,Scannable)
import qualified RPC.DB      as RPC
import qualified RPC.Session as RPC

import Control.Concurrent
    (threadDelay,Chan,newChan,readChan,writeChan,newEmptyMVar,takeMVar,putMVar)
import Control.Concurrent.Async (async,link)
import Control.Monad (unless,guard)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Polling Interface -----------------------------------------------------------

type ControlHandle = Chan (Either ControlMsg InputMsg)

-- | Create the polling thread, and link it to the current thread.
pollServices :: Con Server -> RPC.Token -> IO ControlHandle
pollServices host tok = do
  controlChan <- newChan
  link =<< async (control controlChan)
  link =<< async (watchHosts host tok controlChan)
  link =<< async (watchServices host tok controlChan)
  link =<< async (watchCreds host tok controlChan)
  link =<< async (watchLoot host tok controlChan)
  link =<< async (watchSessions host tok controlChan)
  return controlChan


-- External Interface ----------------------------------------------------------

sendControlMsg :: ControlHandle -> ControlMsg -> IO ()
sendControlMsg chan = writeChan chan . Left

addHostHandler :: ControlHandle -> Handler RPC.HostInfo -> IO HandlerRef
addHostHandler cont h = do
  done <- newEmptyMVar
  sendControlMsg cont (AddHostHandler h (putMVar done))
  takeMVar done

remHostHandler :: ControlHandle -> HandlerRef -> IO ()
remHostHandler cont ref = sendControlMsg cont (RemHostHandler ref)

addServiceHandler :: ControlHandle -> Handler RPC.Service -> IO HandlerRef
addServiceHandler cont h = do
  done <- newEmptyMVar
  sendControlMsg cont (AddServiceHandler h (putMVar done))
  takeMVar done

remServiceHandler :: ControlHandle -> HandlerRef -> IO ()
remServiceHandler cont ref = sendControlMsg cont (RemServiceHandler ref)

addCredHandler :: ControlHandle -> Handler RPC.Cred -> IO HandlerRef
addCredHandler cont h = do
  done <- newEmptyMVar
  sendControlMsg cont (AddCredHandler h (putMVar done))
  takeMVar done

remCredHandler :: ControlHandle -> HandlerRef -> IO ()
remCredHandler cont ref = sendControlMsg cont (RemCredHandler ref)

addLootHandler :: ControlHandle -> Handler RPC.Loot -> IO HandlerRef
addLootHandler cont h = do
  done <- newEmptyMVar
  sendControlMsg cont (AddLootHandler h (putMVar done))
  takeMVar done

remLootHandler :: ControlHandle -> HandlerRef -> IO ()
remLootHandler cont ref = sendControlMsg cont (RemLootHandler ref)

addSessionHandler :: ControlHandle -> Handler (RPC.SessionId,RPC.Session)
                  -> IO HandlerRef
addSessionHandler cont h = do
  done <- newEmptyMVar
  sendControlMsg cont (AddSessionHandler h (putMVar done))
  takeMVar done

remSessionHandler :: ControlHandle -> HandlerRef -> IO ()
remSessionHandler cont ref = sendControlMsg cont (RemSessionHandler ref)


-- Control Interface -----------------------------------------------------------

data Handlers = Handlers
  { handleHost    :: HandlerMap RPC.HostInfo
  , handleService :: HandlerMap RPC.Service
  , handleCred    :: HandlerMap RPC.Cred
  , handleLoot    :: HandlerMap RPC.Loot
  , handleSession :: HandlerMap (RPC.SessionId,RPC.Session)
  }

emptyHandlers :: Handlers
emptyHandlers  = Handlers
  { handleHost    = emptyHandlerMap
  , handleService = emptyHandlerMap
  , handleCred    = emptyHandlerMap
  , handleLoot    = emptyHandlerMap
  , handleSession = emptyHandlerMap
  }

-- | Dispatch new data, and maintain a list of handlers.
control :: ControlHandle -> IO ()
control chan = loop emptyHandlers
  where
  loop hs = do
    e <- readChan chan
    case e of

      -- control messages
      Left msg  -> do
        hs'   <- handleContMsg hs msg
        loop hs'

      -- data messages
      Right msg -> do
        handleInpMsg hs msg
        loop hs


data ControlMsg
  = AddHostHandler (Handler RPC.HostInfo) (HandlerRef -> IO ())
  | RemHostHandler HandlerRef
  | AddServiceHandler (Handler RPC.Service) (HandlerRef -> IO ())
  | RemServiceHandler HandlerRef
  | AddCredHandler (Handler RPC.Cred) (HandlerRef -> IO ())
  | RemCredHandler HandlerRef
  | AddLootHandler (Handler RPC.Loot) (HandlerRef -> IO ())
  | RemLootHandler HandlerRef
  | AddSessionHandler (Handler (RPC.SessionId,RPC.Session)) (HandlerRef -> IO ())
  | RemSessionHandler HandlerRef

-- | Update the state of the handlers, according to a control message.
handleContMsg :: Handlers -> ControlMsg -> IO Handlers
handleContMsg hs msg = case msg of

  AddHostHandler h k -> do
    let (ref,host') = addHandler h (handleHost hs)
    k ref
    return hs { handleHost = host' }

  RemHostHandler ref ->
    return hs { handleHost = removeHandler ref (handleHost hs) }

  AddServiceHandler h k -> do
    let (ref,svcs') = addHandler h (handleService hs)
    k ref
    return hs { handleService = svcs' }

  RemServiceHandler ref ->
    return hs { handleService = removeHandler ref (handleService hs) }

  AddLootHandler h k -> do
    let (ref,svcs') = addHandler h (handleLoot hs)
    k ref
    return hs { handleLoot = svcs' }

  RemLootHandler ref ->
    return hs { handleLoot = removeHandler ref (handleLoot hs) }

  AddCredHandler h k -> do
    let (ref,svcs') = addHandler h (handleCred hs)
    k ref
    return hs { handleCred = svcs' }

  RemCredHandler ref ->
    return hs { handleCred = removeHandler ref (handleCred hs) }

  AddSessionHandler h k -> do
    let (ref,svcs') = addHandler h (handleSession hs)
    k ref
    return hs { handleSession = svcs' }

  RemSessionHandler ref ->
    return hs { handleSession = removeHandler ref (handleSession hs) }


-- Service Polling -------------------------------------------------------------

sendInputMsg :: ControlHandle -> InputMsg -> IO ()
sendInputMsg chan = writeChan chan . Right

type Millis = Int

-- | Delay by some number of milliseconds.
delay :: Millis -> IO ()
delay ms = threadDelay (ms * 1000)

data InputMsg
  = NewHosts [RPC.HostInfo]
  | NewServices [RPC.Service]
  | NewCreds [RPC.Cred]
  | NewLoots [RPC.Loot]
  | NewSessions [(RPC.SessionId,RPC.Session)]
    deriving (Show)

handleInpMsg :: Handlers -> InputMsg -> IO ()
handleInpMsg hs msg = case msg of
  NewHosts hosts   -> mapM_ (dispatchHandlers (handleHost hs)) hosts
  NewServices svcs -> mapM_ (dispatchHandlers (handleService hs)) svcs
  NewCreds creds   -> mapM_ (dispatchHandlers (handleCred hs)) creds
  NewLoots loots   -> mapM_ (dispatchHandlers (handleLoot hs)) loots
  NewSessions sess -> mapM_ (dispatchHandlers (handleSession hs)) sess


-- Hosts -----------------------------------------------------------------------

-- | Poll the db.hosts service, looking for any changes.
--
-- XXX this only watches for new hosts, if host information changes, this won't
-- pick up on that.
watchHosts :: Con Server -> RPC.Token -> ControlHandle -> IO ()
watchHosts host auth chan = loop Set.empty
  where

  loop knownHosts = do
    hosts <- RPC.db_hosts host auth

    let names    = map RPC.hostAddress hosts
        newHosts = filter ((`Set.notMember` knownHosts) . RPC.hostAddress) hosts
    unless (null newHosts) (sendInputMsg chan (NewHosts newHosts))

    delay 2500
    loop (Set.fromList names)


-- Services --------------------------------------------------------------------

data ServiceHash = ServiceHash
  { shHost :: Host Scannable
  , shPort :: Int
  } deriving (Show,Eq,Ord)

-- | Generate a service hash from a service.
mkServiceHash :: RPC.Service -> ServiceHash
mkServiceHash svc = ServiceHash
  { shHost = RPC.serviceHost svc
  , shPort = RPC.servicePort svc
  }

-- | Generate a new service hash, and prevent existing services from bubbling
-- out as new.
checkNewService :: Set.Set ServiceHash -> RPC.Service
                -> (ServiceHash,Maybe RPC.Service)
checkNewService known svc = (hash, mb)
  where
  hash = mkServiceHash svc
  mb   = guard (hash `Set.notMember` known) >> return svc

-- | Poll the the db.services service, looking for changes.
watchServices :: Con Server -> RPC.Token -> ControlHandle -> IO ()
watchServices host auth chan = loop Set.empty
  where
  loop known = do
    svcs <- RPC.db_services host auth
    let (known',svcs') = unzip (map (checkNewService known) svcs)
        newServices    = catMaybes svcs'
    unless (null newServices) (sendInputMsg chan (NewServices newServices))
    delay 10000
    loop (Set.fromList known')


-- Credentials -----------------------------------------------------------------

watchCreds :: Con Server -> RPC.Token -> ControlHandle -> IO ()
watchCreds host auth chan = loop Set.empty
  where
  loop known = do
    creds <- RPC.db_creds host auth

    let newCreds = filter (`Set.notMember` known) creds
    unless (null newCreds) (sendInputMsg chan (NewCreds newCreds))

    delay 30000
    loop (Set.fromList creds)


-- Loot ------------------------------------------------------------------------

watchLoot :: Con Server -> RPC.Token -> ControlHandle -> IO ()
watchLoot host auth chan = loop Set.empty
  where
  loop known = do
    loots <- RPC.db_loots host auth

    let newLoots = filter (`Set.notMember` known) loots
    unless (null newLoots) (sendInputMsg chan (NewLoots newLoots))

    delay 30000
    loop (Set.fromList loots)


-- Sessions --------------------------------------------------------------------

watchSessions :: Con Server -> RPC.Token -> ControlHandle -> IO ()
watchSessions host auth chan = loop Set.empty
  where
  loop known = do
    RPC.SessionMap sl <- RPC.session_list host auth

    let checked = Map.filterWithKey (\k _ -> k `Set.notMember` known) sl
    unless (Map.null checked) (sendInputMsg chan (NewSessions (Map.toList checked)))

    delay 2000
    loop (Set.fromList (Map.keys sl))
