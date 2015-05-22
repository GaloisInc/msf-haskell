-- |Interact with metasploit sessions. e.g. once a host has
-- successfully been exploited, use 'session_shell_read' to send
-- commands to its terminal. These functions often need an extra
-- newline as if you typed them on the terminal.
module RPC.Session
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
  ) where

import MSF.Host (Con,Server)
import Types.Session

-- | Silent operation.
session_list :: Con Server -> Token -> IO SessionMap
session_list addr auth = send_request "session.list" addr
  [ toObject auth
  ]

-- | Quiet operation.
session_stop :: Con Server -> Token -> SessionId -> IO ()
session_stop addr auth sessionID = success "session.stop" =<< send_request "session.stop" addr
  [ toObject auth
  , toObject sessionID
  ]

-- | Silent operaiton, though how it determines compatibility is not clear.
session_compatible_modules :: Con Server -> Token -> SessionId -> IO Modules
session_compatible_modules addr auth sessionID = send_request "session.compatible_modules" addr
  [ toObject auth
  , toObject sessionID
  ]

-- |Quiet operation.
session_shell_read :: Con Server -> Token -> SessionId -> Maybe String -> IO ShellRead
session_shell_read addr auth sessionID mb = send_request "session.shell_read" addr (req ++ extra)
  where
  req = [ toObject auth
        , toObject sessionID
        ]
  -- wrap the optional parameter
  extra = maybe [] (return . toObject) mb

-- |Loud operation.
session_shell_write :: Con Server -> Token -> SessionId -> String -> IO WriteCount
session_shell_write addr auth sessionID input = field "session.shell_write" "write_count" =<< send_request "session.shell_write" addr
  [ toObject auth
  , toObject sessionID
  , toObject input
  ]

-- | Loud operation.
session_shell_upgrade :: Con Server -> Token -> SessionId -> HostName -> PortNumber -> IO ()
session_shell_upgrade addr auth sessionID connectHost connectPort =
  success "session.shell_upgrade" =<< send_request "session.shell_upgrade" addr
    [ toObject auth
    , toObject sessionID
    , toObject connectHost
    , toObject connectPort
    ]

-- Let's assume that all interactions with meterpreter are loud.

-- |Loud operation. 
session_meterpreter_write :: Con Server -> Token -> SessionId -> String -> IO ()
session_meterpreter_write addr auth sessionID input = success "session.meterpreter_write" =<< send_request "session.meterpreter_write" addr
  [ toObject auth
  , toObject sessionID
  , toObject input
  ]

-- NOTE KC: actually quiet? or are we assuming that all interactions with meterpreter are loud?
-- |Quiet operation.
session_meterpreter_read :: Con Server -> Token -> SessionId -> IO String
session_meterpreter_read addr auth sessionID = field "session.meterpreter_read" "data" =<< send_request "session.meterpreter_read" addr
  [ toObject auth
  , toObject sessionID
  ]

-- |Loud operation.
session_meterpreter_run_single :: Con Server -> Token -> SessionId -> String -> IO ()
session_meterpreter_run_single addr auth sessionID command = success "session.meterpreter_run_single" =<< send_request "session.meterpreter_run_single" addr
  [ toObject auth
  , toObject sessionID
  , toObject command
  ]

-- |Loud operation.
session_meterpreter_script :: Con Server -> Token -> SessionId -> String -> IO ()
session_meterpreter_script addr auth sessionID scriptName = success "session.meterpreter_script" =<< send_request "session.meterpreter_script" addr
  [ toObject auth
  , toObject sessionID
  , toObject scriptName
  ]

-- |Quiet operation.
session_meterpreter_session_detach :: Con Server -> Token -> SessionId -> IO Result
session_meterpreter_session_detach addr auth sessionID = send_request "session.meterpreter_session_detach" addr
  [ toObject auth
  , toObject sessionID
  ]

-- |Quiet operation.
session_meterpreter_session_kill :: Con Server -> Token -> SessionId -> IO Result
session_meterpreter_session_kill addr auth sessionID =  send_request "session.meterpreter_session_kill" addr
  [ toObject auth
  , toObject sessionID
  ]

-- |Silent operation, probably.
session_meterpreter_tabs :: Con Server -> Token -> SessionId -> String -> IO Tabs
session_meterpreter_tabs addr auth sessionID line = field "session.meterpreter_tabs" "tabs" =<< send_request "session.meterpreter_tabs" addr
  [ toObject auth
  , toObject sessionID
  , toObject line
  ]

-- |Silent operation, probably.
session_meterpreter_directory_separator :: Con Server -> Token -> SessionId -> IO DirSep
session_meterpreter_directory_separator addr auth sessionID = field "session.meterpreter_directory_separator" "separator" =<< send_request "session.meterpreter_directory_separator" addr
  [ toObject auth
  , toObject sessionID
  ]

--------------------------------------------------------------------------------


{-
ring_clear :: Con Server -> Token -> SessionId -> IO Success
ring_clear addr auth sessionID = send_request "session.ring_clear" addr
  [ toObject auth
  , toObject sessionID
  ]


data RingLast = RingLast
  { getRingLast :: Int
  } deriving (Show)

instance FromObject RingLast where
  fromObject obj = do
    m <- fromObject obj
    RingLast <$> (fromObject =<< Map.lookup "seq" m)

ring_last :: Con Server -> Token -> String -> IO RingLast
ring_last addr auth sessionID = send_request "session.ring_last" addr
  [ toObject auth
  , toObject sessionID
  ]


data Ring_PutReturn
 = Ring_PutSuccess String
 | Ring_PutError

-- XXX is this the right call?
ring_put :: Con Server -> Token -> String -> String -> IO Ring_PutReturn
ring_put addr auth sessionID input =
  let request = arr [str "session.shell_write", str auth, str sessionID, str input] in
  do response <- send_request request
     return (if hasKey response (str "write_count")
       then
         Ring_PutSuccess (str' . fromJust $ (lookup response (str "write_count")))
       else
         Ring_PutError)

data Ring_ReadReturn
 = Ring_ReadSuccess (Maybe String) String
 | Ring_ReadError

-- XXX is this the right call?
ring_read :: Con Server -> Token -> String -> Maybe String -> IO Ring_ReadReturn
ring_read addr auth sessionID (Just readPointer) =
  let request = arr [str "session.shell_read", str auth, str sessionID, str readPointer] in
  do response <- send_request request
     return (if hasKey response (str "data") 
       then
         let seq  = str' <$> lookup response (str "seq")
             output = str' . fromJust $ (lookup response (str "data")) in
         Ring_ReadSuccess seq output
       else
         Ring_ReadError)

-- XXX is this the right call?
ring_read addr auth sessionID Nothing =
  let request = arr [str "session.shell_read", str auth, str sessionID] in
  do response <- send_request request
     return (if hasKey response (str "data") 
       then
         let seq    = str' <$> lookup response (str "seq")
             output = str' . fromJust $ (lookup response (str "data")) in
         Ring_ReadSuccess seq output
       else
         Ring_ReadError)
         -}
