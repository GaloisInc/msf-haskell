module Types.Module
  ( module RPC.Util
  , Payload (..)
  , Payloads
  , Sessions
  , ModuleEncode
  , ExecuteOptions
  , EncodeOptions (..)
  , ModuleOptions (..)
  , ModuleOption (..)
  , ModuleInfo (..)
  , ExecResult (..)
  , ModuleName (..)
  , ModuleType (..)
  ) where

import Types.Job (JobId)
import RPC.Util

import Control.Applicative ((<$>),(<*>),(<|>))
import qualified Data.ByteString as S
import qualified Data.Map as Map
import Data.Maybe

--------------------------------------------------------------------------------

newtype Payload = Payload
  { payload :: String
  } deriving (Show,Eq,Ord)

instance FromObject Payload where
  fromObject obj = Payload <$> fromObject obj

instance ToObject Payload where
  toObject (Payload s) = toObject s

--------------------------------------------------------------------------------

data ModuleInfo = ModuleInfo
  { moduleInfoName          :: ModuleName   
  , moduleInfoDescription   :: String
  , moduleInfoLicense       :: String
  , moduleInfoFilePath      :: String
  , moduleInfoVersion       :: String
  , moduleInfoRank          :: Int
  , moduleInfoReferences    :: [String]
  , moduleInfoAuthors       :: [String]
  , moduleInfoTargets       :: Optional [String]
  , moduleInfoDefaultTarget :: Optional String
  , moduleInfoActions       :: Optional [String]
  , moduleInfoDefaultAction :: Optional String
  } deriving (Show,Eq,Ord)

instance FromObject ModuleInfo where
  fromObject obj = ModuleInfo <$>
    lookupField "name"           obj <*>
    lookupField "description"    obj <*>
    lookupField "license"        obj <*>
    lookupField "filepath"       obj <*>
    lookupField "version"        obj <*>
    lookupField "rank"           obj <*>
    lookupField "references"     obj <*>
    lookupField "authors"        obj <*>
    lookupField "targets"        obj <*>
    lookupField "default_target" obj <*>
    lookupField "actions"        obj <*>
    lookupField "default_action" obj

--------------------------------------------------------------------------------

data EncodeOptions = EncodeOptions
  { encodeFormat :: Optional String
  , encodeBadChars :: Optional String
  , encodePlatform :: Optional String
  , encodeArch :: Optional String
  , encodeECount :: Optional Int
  } deriving (Show,Eq,Ord)

instance ToObject EncodeOptions where
  toObject (EncodeOptions f b p a e) = toObject $ Map.fromList $
    catMaybes
      [ optionalToObject "format"   f
      , optionalToObject "badchars" b
      , optionalToObject "platform" p
      , optionalToObject "arch"     a
      , optionalToObject "ecount"   e
      ]

instance FromObject EncodeOptions where
  fromObject obj = EncodeOptions <$>
    lookupField "format"   obj <*>
    lookupField "badchars" obj <*>
    lookupField "platform" obj <*>
    lookupField "arch"     obj <*>
    lookupField "ecount"   obj

type ExecuteOptions = Object

--------------------------------------------------------------------------------

newtype ModuleOptions = ModuleOptions
  { moduleOptions :: Map.Map String ModuleOption
  } deriving (Show,Eq,Ord)

instance FromObject ModuleOptions where
  fromObject obj = ModuleOptions <$> fromObject obj

--------------------------------------------------------------------------------

data ModuleOption = ModuleOption
  { moduleOptionType     :: String
  , moduleOptionRequired :: Bool
  , moduleOptionAdvanced :: Bool
  , moduleOptionEvasion  :: Bool
  , moduleOptionDesc     :: String
  -- TODO: looks like default can be string or false
  , moduleOptionDefault  :: Optional Object
  , moduleOptionEnums    :: Optional [String]
  } deriving (Show,Eq,Ord)

instance ToObject ModuleOption where
  toObject (ModuleOption t r a ev de df en) = toObject $ Map.fromList $
    catMaybes
      [ Just ( "type"     , toObject t  )
      , Just ( "required" , toObject r  )
      , Just ( "advanced" , toObject a  )
      , Just ( "evasion"  , toObject ev )
      , Just ( "desc"     , toObject de )
      , optionalToObject "default"    df
      , optionalToObject "enums"      en
      ]

instance FromObject ModuleOption where
  fromObject obj = ModuleOption <$>
    lookupField "type"     obj <*>
    lookupField "required" obj <*>
    lookupField "advanced" obj <*>
    lookupField "evasion"  obj <*>
    lookupField "desc"     obj <*>
    lookupField "default"  obj <*>
    lookupField "enums"    obj

--------------------------------------------------------------------------------

data ExecResult
  = ExecJobId JobId
  | ExecPayload S.ByteString
    deriving (Show,Eq,Ord)

instance FromObject ExecResult where
  fromObject obj =
    (ExecJobId <$> lookupField "job_id" obj) <|>
    (ExecPayload <$> lookupField "payload" obj)

--------------------------------------------------------------------------------

newtype ModuleName = ModuleName
  { moduleName :: String
  } deriving (Show,Eq,Ord)

instance ToObject ModuleName where
  toObject (ModuleName s) = toObject s

instance FromObject ModuleName where
  fromObject obj = ModuleName <$> fromObject obj

--------------------------------------------------------------------------------

data ModuleType
  = ExploitModuleType
  | AuxiliaryModuleType
  | PostModuleType
  | PayloadModuleType
  | EncoderModuleType
  | NopModuleType
  deriving (Show,Eq,Ord)

instance ToObject ModuleType where
  toObject t = case t of
    ExploitModuleType   -> toObject "exploit"
    AuxiliaryModuleType -> toObject "auxiliary"
    PostModuleType      -> toObject "post"
    PayloadModuleType   -> toObject "payload"
    EncoderModuleType   -> toObject "encoder"
    NopModuleType       -> toObject "nop"

instance FromObject ModuleType where
  fromObject obj = do
    s <- fromObject obj
    case s of
      "exploit"   -> Just ExploitModuleType  
      "auxiliary" -> Just AuxiliaryModuleType
      "post"      -> Just PostModuleType     
      "payload"   -> Just PayloadModuleType  
      "encoder"   -> Just EncoderModuleType  
      "nop"       -> Just NopModuleType      
      _           -> Nothing

--------------------------------------------------------------------------------

type Payloads = [String]
type Sessions = [String]
type ModuleEncode = String

