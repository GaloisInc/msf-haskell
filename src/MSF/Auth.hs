-- |Authentication related functions.
module MSF.Auth
  ( module Types.Auth
  , auth_login
  , auth_logout
  , auth_token_add
  , auth_token_remove
  , auth_token_generate
  , auth_token_list
  ) where

import MSF.Monad
import Types.Auth
import qualified RPC.Auth as RPC

-- | Login to a given host with user and pass, returning
-- authentication token if successful. Typically use
-- 'MSF.Monad.login'.
auth_login :: (SilentCxt s) => Username -> Password -> MSF s (Either String Token)
auth_login user pass = prim $ \ addr _ ->
  RPC.auth_login addr user pass

-- | Log a given authentication token out. Attempts to diagnose failure.
auth_logout :: (SilentCxt s) => Token -> MSF s ()
auth_logout tok = prim $ \ addr auth ->
  RPC.auth_logout addr auth tok

-- | Add a permanent authentication token.
auth_token_add :: (SilentCxt s) => Token -> MSF s ()
auth_token_add candidate = prim $ \ addr auth ->
  RPC.auth_token_add addr auth candidate

-- | Remove either a temporary or permanent auth token.
auth_token_remove :: (SilentCxt s) => Token -> MSF s ()
auth_token_remove target = prim $ \ addr auth ->
  RPC.auth_token_remove addr auth target

-- | Generate a new authentication token.
auth_token_generate :: (SilentCxt s) => MSF s Token
auth_token_generate = prim RPC.auth_token_generate

-- | Get a list of all the auth tokens.
auth_token_list :: (SilentCxt s) => MSF s [Token]
auth_token_list = prim RPC.auth_token_list

