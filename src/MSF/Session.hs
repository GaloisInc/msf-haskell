-- |Interact with metasploit sessions. e.g. once a host has
-- successfully been exploited, use 'session_shell_read' to send
-- commands to its terminal. These functions often need an extra
-- newline as if you typed them on the terminal.
module MSF.Session
  ( module Types.Session
  , session_list
  , session_stop
  , session_compatible_modules
  , session_shell_read
  , session_shell_write
  , session_shell_upgrade
  , session_meterpreter_write
  , session_meterpreter_read
  , session_meterpreter_run_single
  , session_meterpreter_script
  , session_meterpreter_session_detach
  , session_meterpreter_session_kill
  , session_meterpreter_tabs
  , session_meterpreter_directory_separator
  , sessions_on_host
  ) where

import Types.Session
import qualified RPC.Session as RPC
import MSF.Monad
import MSF.Host
import qualified Data.Map as Map

-- |Retrieve the list of sessions from the MSF server.
session_list :: (SilentCxt s) => MSF s SessionMap
session_list = prim RPC.session_list

-- |Stop this session. Quiet since it interacts with a target.
session_stop :: (QuietCxt s) => SessionId -> MSF s ()
session_stop sid = prim $ \addr auth -> do
  RPC.session_stop addr auth sid

-- |Get the compatible modules for this session.
session_compatible_modules :: (SilentCxt s) => SessionId -> MSF s Modules
session_compatible_modules sessionID = prim $ \ addr auth -> do
  RPC.session_compatible_modules addr auth sessionID

-- |Read the output from this shell. e.g. if you send a command via
-- 'session_shell_write', use this function to read the output.
session_shell_read :: (QuietCxt s) => SessionId -> Maybe String -> MSF s ShellRead
session_shell_read sessionID mb = prim $ \ addr auth -> do
  RPC.session_shell_read addr auth sessionID mb

-- |Run a command or otherwise write something to this
-- session. Commands should terminate with a newline as if you typed
-- them on the terminal.
session_shell_write :: (LoudCxt s) =>  SessionId -> String -> MSF s WriteCount
session_shell_write sessionID input = prim $ \ addr auth -> do
  RPC.session_shell_write addr auth sessionID input

-- |Upgrade this shell to meterpreter.
session_shell_upgrade :: (LoudCxt s) => SessionId -> HostName -> PortNumber -> MSF s ()
session_shell_upgrade sessionID host port = prim $ \ addr auth -> do
  RPC.session_shell_upgrade addr auth sessionID host port

-- |Write to this meterpreter session.
session_meterpreter_write :: (LoudCxt s) => SessionId -> String -> MSF s ()
session_meterpreter_write sessionID input = prim $ \ addr auth -> do
  RPC.session_meterpreter_write addr auth sessionID input

-- |Read from this meterpreter session.
session_meterpreter_read :: (QuietCxt s) => SessionId -> MSF s String
session_meterpreter_read sessionID = prim $ \ addr auth -> do
  RPC.session_meterpreter_read addr auth sessionID

session_meterpreter_run_single :: (LoudCxt s) => SessionId -> String -> MSF s ()
session_meterpreter_run_single sessionID input = prim $ \ addr auth -> do
  RPC.session_meterpreter_run_single addr auth sessionID input

session_meterpreter_script :: (LoudCxt s) => SessionId -> String -> MSF s ()
session_meterpreter_script sessionID input = prim $ \ addr auth -> do
  RPC.session_meterpreter_script addr auth sessionID input

-- |Detatch this meterpreter session.
session_meterpreter_session_detach :: (QuietCxt s) => SessionId -> MSF s Result
session_meterpreter_session_detach sessionID = prim $ \ addr auth -> do
  RPC.session_meterpreter_session_detach addr auth sessionID

-- |Stop this meterpreter session.
session_meterpreter_session_kill :: (QuietCxt s) => SessionId -> MSF s Result
session_meterpreter_session_kill sessionID = prim $ \ addr auth -> do
  RPC.session_meterpreter_session_kill addr auth sessionID

session_meterpreter_tabs :: (SilentCxt s) => SessionId -> String -> MSF s Tabs
session_meterpreter_tabs sessionID input = prim $ \ addr auth -> do
  RPC.session_meterpreter_tabs addr auth sessionID input

session_meterpreter_directory_separator :: (SilentCxt s) => SessionId -> MSF s DirSep
session_meterpreter_directory_separator sessionID = prim $ \ addr auth -> do
  RPC.session_meterpreter_directory_separator addr auth sessionID

-- |Get the session(s) that are open on the given host - layered
-- function, not part of MSF api.
sessions_on_host :: SessionMap -> Host t -> [SessionId]
sessions_on_host sl host = Map.keys (Map.filter p (RPC.getSessionMap sl))
  where
  p sess = RPC.sessTargetHost sess == getHost host
