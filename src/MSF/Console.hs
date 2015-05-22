-- |Interacting with MSF consoles, which is similar to typing commands
-- into Metasploit's interactive msfconsole program.
module MSF.Console
  ( module Types.Console
  , console_create
  , console_destroy
  , console_read
  , console_write
  , console_write_attempts
  , console_tabs
  , console_list
  , console_info
  , console_busy
  , console_session_detach
  , console_session_kill
  ) where

import MSF.Monad
import Types.Console
import qualified RPC.Console as RPC

import Data.List (find)

-- | Create a new console.
console_create :: (SilentCxt s) => MSF s Console
console_create = prim RPC.console_create

-- | Destroy the console that's currently in scope.
console_destroy :: (SilentCxt s) => MSF s Result
console_destroy = do
  k <- console
  prim $ \ addr auth -> RPC.console_destroy addr auth k

-- | Read from the console that's currently in scope.
console_read :: (SilentCxt s) => MSF s (Maybe ConsoleRead)
console_read  = do
  k <- console
  prim $ \ addr auth -> RPC.console_read addr auth k

-- |Get a list of all of the open consoles
console_list :: SilentCxt s => MSF s [Console]
console_list  = prim RPC.console_list

-- |Get more information abut the given console.
console_info :: SilentCxt s => MSF s (Maybe Console)
console_info  = do
  list <- console_list
  k    <- console
  return (find (\c -> consoleId c == k) list)

-- | Check if the current console is busy.
console_busy :: SilentCxt s => MSF s (Maybe Bool)
console_busy  = fmap consoleBusy `fmap` console_info

-- | Write to the console that's currently in scope. Defaults to ten attempts.
console_write :: (QuietCxt s) => String -> MSF s ()
console_write = console_write_attempts 10

-- | Attempts to write to the console a given number of times.
console_write_attempts :: (QuietCxt s) => Int -> String -> MSF s ()
console_write_attempts atmpts input
  | atmpts == 0 = io $ writeFailed input
  | otherwise = do
    k <- console
    writtenLen <- prim $ \ addr auth -> RPC.console_write addr auth k input
    case writtenLen of
      Just wl
        | length input == wl -> return ()
        | otherwise          -> console_write_attempts atmpts $ drop wl input
      Nothing                -> console_write_attempts (atmpts - 1) input 

-- | Get tab completion on input line from current console session.
console_tabs :: (SilentCxt s) => String -> MSF s (Maybe Tabs)
console_tabs input = do
  k <- console
  prim $ \ addr auth -> do
    RPC.console_tabs addr auth k input

-- | Background the console session currently in scope.
console_session_detach :: (QuietCxt s) => MSF s Result
console_session_detach = do
  k <- console
  prim $ \ addr auth -> do
    RPC.console_session_detach addr auth k

-- | Kill the current console session.
console_session_kill :: (QuietCxt s) => MSF s Result
console_session_kill = do
  k <- console
  prim $ \ addr auth -> do
    RPC.console_session_kill addr auth k

