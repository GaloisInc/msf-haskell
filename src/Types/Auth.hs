{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Auth
  ( module RPC.Util
  , LoginResult (..)
  , TokenList (..)
  , Username
  , Password
  ) where

import RPC.Util

import Control.Applicative ((<$>),(<|>))

--------------------------------------------------

data LoginResult
  = LoginSuccess Token
  | LoginFailure String
  deriving (Show,Eq,Ord)

instance FromObject LoginResult where
  fromObject obj =
    (LoginSuccess <$> lookupField "token" obj) <|>
    (LoginFailure <$> lookupField "error_message" obj)

--------------------------------------------------

newtype TokenList = TokenList
  { tokenList :: [Token]
  } deriving Show

instance FromObject TokenList where
  fromObject obj = TokenList <$> lookupField "tokens" obj

--------------------------------------------------

type Username = String
type Password = String

