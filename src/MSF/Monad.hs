-- |The MSF monad encapsulates state, including logged-in credentials
-- and enforces the volume (silent, quiet, loud) context.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
--{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE KindSignatures #-}

module MSF.Monad where

import MSF.Event.Prim
import MSF.Host

import qualified RPC.Auth    as RPC
import qualified RPC.Console as RPC

import Control.Applicative (Applicative, (<$>))

import Data.Typeable (Typeable)
import MonadLib (ReaderT,runM,ask,inBase)
import qualified Control.Exception as X

--data Volume = Silent | Quiet | Loud
data Silent
data Quiet
data Loud

-- |A silent operation is one which interacts with the metasploit
-- server, but does not itself directly cause any action that
-- interacts with a target. For example, logging into the metasploit
-- server or listing open sessions.  This class covers *volume* parameters that
-- will support silent operations.
class SilentCxt s
--class SilentCxt (s :: Volume)
instance SilentCxt Silent
instance SilentCxt Quiet
instance SilentCxt Loud

-- |A quiet operation *can* impact a target, like closing a session if one
-- is already open, and some types of "quiet" port scans.  This class covers
-- *volume* parameters that support quiet operations.
class SilentCxt s => QuietCxt s
--class SilentCxt s => QuietCxt (s :: Volume)
instance QuietCxt Quiet
instance QuietCxt Loud

-- |A loud operation is one which seems so to us would likely be
-- detected by an IDS. This is just our best guess. This includes loud
-- nmap scans and launching exploits. Of course, some modules are
-- probably quiet.  This class covers *volume* parameters that support loud
-- operations.
class QuietCxt s => LoudCxt s
--class QuietCxt s => LoudCxt (s :: Volume)
instance LoudCxt Loud

-- |The MSF monad encapsulates some state and enforces the
-- *volume* (silent, quiet, loud) context.
newtype MSF s a = MSF
  { unMSF :: ReaderT Env IO a
  } deriving (Functor,Applicative,Monad)

-- |Execute this MSF monad with the given environment. Typically use
-- "login" instead.
runMSF :: MSF s a -> Env -> IO a
runMSF  = runM . unMSF

-- |The internal environment encapsulated in the MSF monad.
data Env = Env
  { envCon     :: Con Server    -- ^ The Metasploit server we communicate with
  , envToken   :: RPC.Token     -- ^ The logged-in authentication token
  , envConsole :: RPC.ConsoleId -- ^ A console to interact with
  , envControl :: ControlHandle
  }

data LoginFailure = LoginFailure
    deriving (Show,Typeable)

instance X.Exception LoginFailure


-- | Knowingly begin a @Loud@ operation. Converts e.g. MSF Loud a
-- into MSF Quiet a.
loud :: MSF Loud a -> MSF s a
loud (MSF m) = MSF m

-- | Perform some IO in the @MSF@ monad.
io :: IO a -> MSF s a
io  = MSF . inBase

-- | Lift an RPC primitive in to the @MSF@ monad.
--
-- XXX this is dangerous to give to users, as it gives a way to circumvent the
-- volume parameter.
prim :: (Con Server -> RPC.Token -> IO a) -> MSF s a
prim k = MSF $ do
  env <- ask
  inBase (k (envCon env) (envToken env))

-- | Retrieve the current console.
console :: MSF s RPC.ConsoleId
console  = MSF (envConsole <$> ask)

-- | Retrieve the current control handle.
controlHandle :: MSF s ControlHandle
controlHandle  = MSF (envControl <$> ask)

-- | Generate a local run function, that fixes the environment.
embed :: MSF s' (MSF s () -> IO ())
embed  = MSF $ do
  env <- ask
  return (\ m -> runMSF m env)

-- | Run an MSF computation, in the context of the RPC
-- server. Typically use this instead of 'MSF.auth_login'.
login :: QuietCxt s => Con Server -> String -> String -> MSF s a -> IO a
login con user pass body = do
  result <- RPC.auth_login con user pass
  case result of

    Right tok -> do
      k    <- RPC.console_create con tok
      _    <- RPC.console_read con tok (RPC.consoleId k) -- throw away the banner
      cont <- pollServices con tok
      a    <- runMSF body Env
                { envCon     = con
                , envToken   = tok
                , envConsole = RPC.consoleId k
                , envControl = cont
                }
-- TODO: Broken      RPC.auth_token_remove con tok tok
      return a

    Left _ -> X.throwIO LoginFailure
