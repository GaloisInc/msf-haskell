module Types.DB
  ( module RPC.Util
  , MAC (..)
  , HostList (..)
  , Host (..)
  , NewHost (..)
  , HostInfo (..)
  , ServiceList (..)
  , Service (..)
  , NewService (..)
  , CredList (..)
  , Cred (..)
  , NewCred (..)
  , LootList (..)
  , Loot (..)
  , NewLoot (..)
  , NewVuln (..)
  ) where

import RPC.Util

import MSF.Host (Host,Scannable)
import qualified MSF.Host as MSF

import Control.Applicative ((<$>),(<*>))
import Data.Maybe
import qualified Data.Map as Map

--------------------------------------------------------------------------------

newtype MAC = MAC
  { getMAC :: String
  } deriving (Show,Eq,Ord)

instance ToObject MAC where
  toObject (MAC m) = toObject m

instance FromObject MAC where
  fromObject obj = MAC <$> fromObject obj

--------------------------------------------------------------------------------

newtype HostList = HostList
  { getHostList :: [HostInfo]
  } deriving (Eq,Ord,Show)

instance FromObject HostList where
  fromObject obj = HostList <$> lookupField "hosts" obj

--------------------------------------------------------------------------------

data NewHost = NewHost
  { newHostAddr        :: String
  , newHostState       :: Optional String
  , newHostOsName      :: Optional String
  , newHostOsFlavor    :: Optional String
  , newHostOsSP        :: Optional String
  , newHostOsLang      :: Optional String
  , newHostArch        :: Optional String
  , newHostMAC         :: Optional MAC
  , newHostScope       :: Optional String
  , newHostVirtualHost :: Optional String
  } deriving (Show,Eq,Ord)

instance ToObject NewHost where
  toObject (NewHost ad st on fl os ol ar mc sc vh) =
    toObject $ Map.fromList $ catMaybes
      [ Just ( "address" , toObject ad )
      , optionalToObject "state"        st
      , optionalToObject "os_name"      on
      , optionalToObject "os_flavor"    fl
      , optionalToObject "os_sp"        os
      , optionalToObject "os_lang"      ol
      , optionalToObject "arch"         ar
      , optionalToObject "mac"          mc
      , optionalToObject "scope"        sc
      , optionalToObject "virtual_host" vh
      ]

--------------------------------------------------------------------------------

data NewService = NewService
  { newServiceHost  :: String
  , newServicePort  :: String
  , newServiceProto :: String
  , newServiceName  :: Optional String
  } deriving (Show,Eq,Ord)

instance ToObject NewService where
  toObject (NewService ho po pr nm) =
    toObject $ Map.fromList $ catMaybes
      [ Just ( "host" , toObject ho  )
      , Just ( "port" , toObject po  )
      , Just ( "proto" , toObject pr )
      , optionalToObject "name" nm
      ]

--------------------------------------------------------------------------------

data NewCred = NewCred
  { newCredHost     :: String
  , newCredPort     :: String
  , newCredUser     :: Optional String
  , newCredPass     :: Optional String
  , newCredPassType :: Optional String
  , newCredProto    :: Optional String
  , newCredActive   :: Optional Bool
  , newCredProof    :: Optional String
  } deriving (Show,Eq,Ord)

instance ToObject NewCred where
  toObject (NewCred hs po us as pt pr ac pf) =
    toObject $ Map.fromList $ catMaybes
      [ Just ( "host" , toObject hs )
      , Just ( "port" , toObject po )
      , optionalToObject "user"   us
      , optionalToObject "pass"   as
      , optionalToObject "ptype"  pt
      , optionalToObject "proto"  pr
      , optionalToObject "active" ac
      , optionalToObject "proof"  pf
      ]

--------------------------------------------------------------------------------

data NewLoot = NewLoot
  { newLootPath        :: String
  , newLootType        :: String
  , newLootHost        :: Optional String
  , newLootContentType :: Optional String
  , newLootName        :: Optional String
  , newLootInfo        :: Optional String
  , newLootData        :: Optional String
  , newLootService     :: Optional String
  } deriving (Show,Eq,Ord)

instance ToObject NewLoot where
  toObject (NewLoot pa ty ho ct nm io da sr) =
    toObject $ Map.fromList $ catMaybes
      [ Just ( "path" , toObject pa )
      , Just ( "type" , toObject ty )
      , optionalToObject "host"    ho
      , optionalToObject "ctype"   ct
      , optionalToObject "name"    nm
      , optionalToObject "info"    io
      , optionalToObject "data"    da
      , optionalToObject "service" sr
      ]

--------------------------------------------------------------------------------

data NewVuln = NewVuln
  { newVulnHost    :: String
  , newVulnName    :: String
  , newVulnInfo    :: Optional String
  , newVulnRefs    :: Optional String
  , newVulnDetails :: Optional String
  } deriving (Show,Eq,Ord)

instance ToObject NewVuln where
  toObject (NewVuln ho nm io rf de) =
    toObject $ Map.fromList $ catMaybes
      [ Just ( "host" , toObject ho )
      , Just ( "name" , toObject nm )
      , optionalToObject "info"    io
      , optionalToObject "refs"    rf
      , optionalToObject "details" de
      ]

--------------------------------------------------------------------------------

data HostState
  = HostStateAlive
  | HostStateUnknown
  deriving (Show,Eq,Ord)

--------------------------------------------------------------------------------

data HostInfo = HostInfo
  { hostCreatedAt :: Int
  , hostAddress   :: MSF.Host MSF.Scannable
  , hostMAC       :: MAC
  , hostName      :: String
  , hostState     :: String -- good candidate for a more specific type
  , hostOsName    :: String
  , hostOsFlavor  :: String
  , hostOsSP      :: String
  , hostOsLang    :: String
  , hostUpdatedAt :: Int
  , hostPurpose   :: String
  , hostInfo      :: String
  } deriving (Show,Eq,Ord)

instance FromObject HostInfo where
  fromObject obj = HostInfo <$>
    lookupField "created_at" obj <*>
    lookupField "address"    obj <*>
    lookupField "mac"        obj <*>
    lookupField "name"       obj <*>
    lookupField "state"      obj <*>
    lookupField "os_name"    obj <*>
    lookupField "os_flavor"  obj <*>
    lookupField "os_sp"      obj <*>
    lookupField "os_lang"    obj <*>
    lookupField "updated_at" obj <*>
    lookupField "purpose"    obj <*>
    lookupField "info"       obj

--------------------------------------------------------------------------------

newtype ServiceList = ServiceList
  { getServiceList :: [Service]
  } deriving (Eq,Ord,Show)

instance FromObject ServiceList where
  fromObject obj = ServiceList <$> lookupField "services" obj

--------------------------------------------------------------------------------

data Service = Service
  { serviceHost      :: MSF.Host MSF.Scannable
  , serviceCreatedAt :: Int
  , serviceUpdatedAt :: Int
  , servicePort      :: Int
  , serviceProto     :: String
  , serviceState     :: String -- good candidate for a more specific type
  , serviceName      :: String
  , serviceInfo      :: String
  } deriving (Show,Eq,Ord)

instance FromObject Service where
  fromObject obj = Service <$>
    lookupField "host"       obj <*>
    lookupField "created_at" obj <*>
    lookupField "updated_at" obj <*>
    lookupField "port"       obj <*>
    lookupField "proto"      obj <*>
    lookupField "state"      obj <*>
    lookupField "name"       obj <*>
    lookupField "info"       obj

--------------------------------------------------------------------------------

newtype CredList = CredList
  { getCreds :: [Cred]
  } deriving (Eq,Ord,Show)

instance FromObject CredList where
  fromObject obj = CredList <$> lookupField "creds" obj

--------------------------------------------------------------------------------

data Cred = Cred
  { credHost        :: Host Scannable
  , credUpdatedAt   :: Int
  , credPort        :: Int
  , credProto       :: String
  , credServiceName :: Object
  , credType        :: String
  , credUser        :: String
  , credPass        :: String
  , credActive      :: Bool
  } deriving (Show,Eq,Ord)

instance FromObject Cred where
  fromObject obj = Cred <$>
    lookupField "host"       obj <*>
    lookupField "updated_at" obj <*>
    lookupField "port"       obj <*>
    lookupField "proto"      obj <*>
    -- TODO Looks like sname can be nilobject among other things
    lookupField "sname"      obj <*>
    lookupField "type"       obj <*>
    lookupField "user"       obj <*>
    lookupField "pass"       obj <*>
    lookupField "active"     obj

--------------------------------------------------------------------------------

newtype LootList = LootList
  { getLootList :: [Loot]
  } deriving (Show)

instance FromObject LootList where
  fromObject obj = LootList <$> lookupField "loots" obj

data Loot = Loot
  { lootHost        :: Host Scannable
  , lootService     :: Optional String
  , lootType        :: String
  , lootContentType :: String
  , lootData        :: Object
  , lootCreatedAt   :: Int
  , lootUpdatedAt   :: Int
  , lootName        :: String
  , lootInfo        :: String
  } deriving (Show,Eq,Ord)

instance FromObject Loot where
  fromObject obj = Loot <$>
    lookupField "host"       obj <*>
    lookupField "service"    obj <*>
    lookupField "ltype"      obj <*>
    lookupField "ctype"      obj <*>
    -- TODO Looks like data can be nilobject among other things
    lookupField "data"       obj <*> 
    lookupField "created_at" obj <*>
    lookupField "updated_at" obj <*>
    lookupField "name"       obj <*>
    lookupField "info"       obj

