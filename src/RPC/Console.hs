-- |Interacting with MSF consoles, which is similar to typing commands
-- into Metasploit's interactive msfconsole program.
module RPC.Console
  ( module Types.Console
  , console_create
  , console_destroy
  , console_read
  , console_write
  , console_tabs
  , console_list
  , console_session_detach
  , console_session_kill
  ) where

import MSF.Host (Con,Server)
--import Types.Session
import Types.Console

-- | Allocate a new console. Silent operation.
console_create :: Con Server -> Token -> IO Console
console_create addr auth = send_request "console.create" addr
  [ toObject auth
  ]

-- | Delete the given console. Should always happen for each open console. Silent operation - I think.
console_destroy :: Con Server -> Token -> ConsoleId -> IO Result
console_destroy addr auth consoleID = send_request "console.destroy" addr
  [ toObject auth
  , toObject consoleID
  ]

-- | Get the unread data from the console (e.g. outpu tof console_write). Silent operation.
console_read :: Con Server -> Token -> ConsoleId -> IO (Maybe ConsoleRead)
console_read addr auth consoleID = send_request "console.read" addr
  [ toObject auth
  , toObject consoleID
  ]

-- | Writes a command to the given console, appending newline char if not already present.
-- Can potentially be a quiet or loud operation, depending on the command.
-- | Returns length of data written.
console_write :: Con Server -> Token -> ConsoleId -> String -> IO (Maybe WriteCount)
console_write addr auth consoleID input = field "console.write" "wrote" =<< send_request "console.write" addr
  [ toObject auth
  , toObject consoleID
  , toObject input
  ]

-- |Emulates tab completion. Silent operation.
console_tabs :: Con Server -> Token -> ConsoleId -> String -> IO (Maybe Tabs)
console_tabs addr auth consoleID inputLine = field "console.tabs" "tabs" =<< send_request "console.tabs" addr
  [ toObject auth
  , toObject consoleID
  , toObject inputLine
  ]

-- | Get a list of all the open consoles. Silent operation.
console_list :: Con Server -> Token -> IO [Console]
console_list addr auth = field "consoles.list" "consoles" =<< send_request "console.list" addr
  [ toObject auth
  ]

-- | Backgrounds an interactive session. Quiet operation.
console_session_detach :: Con Server -> Token -> ConsoleId -> IO Result
console_session_detach addr auth consoleID = send_request "console.session_detach" addr
  [ toObject auth
  , toObject consoleID
  ]

-- | Kill a given session. Quiet operation.
console_session_kill :: Con Server -> Token -> ConsoleId -> IO Result
console_session_kill addr auth consoleID = send_request "console.session_kill" addr
  [ toObject auth
  , toObject consoleID
  ]

