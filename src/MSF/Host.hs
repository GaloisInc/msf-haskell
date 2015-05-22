-- |Various types of hosts that Metasploit might interact
-- with. Particularly, /attackable/ and /scannable/ hosts.
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MSF.Host where

import MsgPack


-- | A metasploit framework RPC server.
data Server

-- | A host that can have attacks launched against it.
data Attackable

-- | A host that can be scanned.
data Scannable


class ScanCxt t
instance ScanCxt Scannable
instance ScanCxt Attackable

class ScanCxt t => AttackCxt t
instance AttackCxt Attackable


-- Individual Hosts ------------------------------------------------------------

-- | A host is just a name or an address.
newtype Host t = Host
  { getHost :: String
  } deriving (Show,Eq,Ord)

-- XXX dangerous, do not export
castHost :: Host t -> Host t'
castHost (Host n) = Host n

attackableHost :: Host Scannable -> Host Attackable
attackableHost  = castHost

instance ToObject (Host t) where
  toObject = toObject . getHost

instance FromObject (Host t) where
  fromObject = fmap Host . fromObject


-- Host Connections ---------------------------------------------------------

-- | A connection is a host and a port. The MSF server itself is a connection.
data Con t = Con
  { conHost :: Host t
  , conPort :: String
  } deriving (Show,Eq)

-- XXX Don't export from a top-level api
getCon :: Con t -> String
getCon con
  | null (conPort con) = getHost (conHost con) ++ ":" ++ conPort con
  | otherwise          = getHost (conHost con)


-- Command Targets ----------------------------------------------------------

-- | One or more targets
newtype Target t = Target
  { getTarget :: [TargetRange t]
  } deriving (Show)

-- XXX dangerous, do not export
castTarget :: Target t -> Target t'
castTarget (Target ts) = Target (map castTargetRange ts)

-- | A range of hosts.
data TargetRange t
  = CIDR (Host t) Int
  | HostRange (Host t) (Host t)
  | SingleHost (Host t)
    deriving (Show)

-- XXX dangerous, do not export
castTargetRange :: TargetRange t -> TargetRange t'
castTargetRange range = case range of
  CIDR h n      -> CIDR (castHost h) n
  HostRange l r -> HostRange (castHost l) (castHost r)
  SingleHost h  -> SingleHost (castHost h)

class HostStage l r t | l r -> t
instance HostStage Attackable Attackable Attackable
instance HostStage Attackable Scannable  Scannable
instance HostStage Scannable Attackable  Scannable
instance HostStage Scannable  Scannable  Scannable

-- | Target combination.  This might need a better operator name at some point.
(&) :: HostStage l r t => Target l -> Target r -> Target t
l & r = Target (getTarget (castTarget l) ++ getTarget (castTarget r))
infixr 1 &

-- | Construct a host range.
to :: Host t -> Host t -> Target t
lo `to` hi = Target [HostRange lo hi]

-- | Construct a host range described by CIDR notation.
cidr :: Host t -> Int -> Target t
cidr h n = Target [CIDR h n]

-- | Lift a single host into a target specification.
single :: Host t -> Target t
single h = Target [SingleHost h]

-- | Convert a @Target@ into an application specific use.
foldTarget :: (TargetRange t -> a -> a) -> a -> Target t -> a
foldTarget fmt z = foldr fmt z . getTarget
