-- |Authentication related functions. Low-level version.
module RPC.Auth
  ( module Types.Auth
  , auth_login
  , auth_logout
  , auth_token_add
  , auth_token_remove
  , auth_token_generate
  , auth_token_list
  ) where

import MSF.Host (Server,Con(..))
import Types.Auth

-- | Log into the metasploit team server with username & password. Silent operation.
auth_login :: Con Server -> Username -> Password -> IO (Either String Token)
auth_login addr username password = send_request "auth.login" addr
  [ toObject username
  , toObject password
  ]

-- |Log a token out. Silent operation.
auth_logout :: Con Server -> Token -> Token -> IO ()
auth_logout addr auth tok = success "auth.logout" =<< send_request "auth.logout" addr
  [ toObject auth
  , toObject tok
  ]

-- | Add a permanent authentication token. Silent operation.
auth_token_add :: Con Server -> Token -> Token -> IO ()
auth_token_add addr auth candidate = success "auth.token_add" =<< send_request "auth.token_add" addr
  [ toObject auth
  , toObject candidate
  ]

-- | Remove either a temporary or perminant token. Silent operation.
auth_token_remove :: Con Server -> Token -> Token -> IO ()
auth_token_remove addr auth target = success "auth.token_remove" =<< send_request "auth.token_remove" addr
  [ toObject auth
  , toObject target
  ]

-- |Create a 32 byte authentication token. Silent operation.
auth_token_generate :: Con Server -> Token -> IO Token
auth_token_generate addr auth = send_request "auth.token_generate" addr
  [ toObject auth
  ]

-- | Get a list of all the authentication tokens. Silent operation.
auth_token_list :: Con Server -> Token -> IO [Token]
auth_token_list addr auth = field "auth.token_list" "tokens" =<< send_request "auth.token_list" addr
  [ toObject auth
  ]

