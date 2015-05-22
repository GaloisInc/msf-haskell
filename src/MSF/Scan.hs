-- |Extended functionality to interact with nmap. These functions
-- aren't exported directly by the Metasploit server, but rather work
-- by calling db_nmap on the console. Compose namp parameters thusly:
-- /db_nmap (tcpSynScan <> tcpConnectScan)/
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module MSF.Scan (
    db_nmap
  , NmapOptions()
  , customScan
  , emptyScan
  , serviceVersionScan
  , tcpSynScan
  , pingScan
  , sctpInitScan
  , tcpConnectScan
  , udpScan
  , tcpNullScan
  , finScan
  , xmasScan
  , tcpAckScan
  , tcpWindowScan
  , tcpMaimonScan
  , sctpCookieEchoScan
  , ipProtocolScan
  , zombieHostScan
  , scanFlagsScan
  , ftpRelayScan
  ) where

import MSF.Host
import MSF.Monad
import RPC.Console

import Data.Monoid (Monoid(..))


-- XXX don't export, this works in all contexts.
nmap_scan :: String -- ^ nmap options
          -> MSF v ()
nmap_scan args = do
  let command = ("db_nmap " ++ args ++ "\n")
  k <- console
  prim $ \ addr auth -> do
    _ <- console_write addr auth k command
    return ()


-- | Run an nmap scan. Use "MSF.Event" to handle callbacks like
-- 'MSF.Event.onHost'.
db_nmap :: ScanCxt t => NmapOptions s -> Target t -> MSF s ()
db_nmap flags tgt =
  nmap_scan (unwords [getNmapOptions flags, foldTarget formatTargetRange "" tgt])

formatTargetRange :: TargetRange t -> String -> String
formatTargetRange range s = case range of
  CIDR h n      -> unwords [getHost h ++ "/" ++ show n, s]
  -- host ranges aren't really applicable for nmap, they use a different
  -- notation for that.
  HostRange _ _ -> error "db_nmap: HostRange" -- XXX
  SingleHost h  -> unwords [getHost h,s]


-- | Options to be supplied to the db_nmap command.
newtype NmapOptions s = NmapOptions
  { getNmapOptions :: String
  } deriving (Show)

instance QuietCxt s => Monoid (NmapOptions s) where
  mempty      = NmapOptions ""
  mappend l r = NmapOptions (getNmapOptions l ++ " " ++ getNmapOptions r)

-- | User-supplied arguments to nmap are assumed to be loud by default.
customScan :: LoudCxt s => String -> NmapOptions s
customScan  = NmapOptions

-- | User-supplied arguments to nmap are assumed to be loud by default.
emptyScan :: LoudCxt s => NmapOptions s
emptyScan  = NmapOptions ""

-- | Service version scanning.
serviceVersionScan :: LoudCxt s => NmapOptions s
serviceVersionScan  = NmapOptions "-sV"

-- | Tcp Syn scanning.
tcpSynScan :: QuietCxt s => NmapOptions s
tcpSynScan  = NmapOptions "-sS"

-- | SCTP Init scanning.
sctpInitScan :: QuietCxt s => NmapOptions s
sctpInitScan  = NmapOptions "-sY"

-- | Tcp Syn scanning.
tcpConnectScan :: LoudCxt s => NmapOptions s
tcpConnectScan  = NmapOptions "-sT"

-- | UDP scanning.
udpScan :: LoudCxt s => NmapOptions s
udpScan  = NmapOptions "-sU"

-- | Ping scan only. Don't perform a port scan.
pingScan :: QuietCxt s => NmapOptions s
pingScan  = NmapOptions "-sP"

-- | Does not set any bits (TCP flag header is 0)
tcpNullScan :: LoudCxt s => NmapOptions s
tcpNullScan  = NmapOptions "-sN"

-- | Sets just the TCP FIN bit.
finScan :: LoudCxt s => NmapOptions s
finScan  = NmapOptions "-sF"

-- | Sets the FIN, PSH, and URG flags, lighting the packet up like a Christmas tree.
xmasScan :: LoudCxt s => NmapOptions s
xmasScan  = NmapOptions "-sX"

-- | This never determines open (or even open|filtered) ports. It is
-- used to map out firewall rulesets, determining whether they are
-- stateful or not and which ports are filtered.
tcpAckScan :: LoudCxt s => NmapOptions s
tcpAckScan  = NmapOptions "-sA"

-- | Window scan is exactly the same as ACK scan except that it
-- exploits an implementation detail of certain systems to
-- differentiate open ports from closed ones, rather than always
-- printing unfiltered when a RST is returned
tcpWindowScan :: LoudCxt s => NmapOptions s
tcpWindowScan  = NmapOptions "-sW"

-- |  This technique is exactly the same as NULL, FIN, and Xmas scans, except that the probe is FIN/ACK. 
tcpMaimonScan :: LoudCxt s => NmapOptions s
tcpMaimonScan = NmapOptions "-sM"

-- | SCTP COOKIE ECHO scan is a more advanced SCTP scan. It takes
-- advantage of the fact that SCTP implementations should silently
-- drop packets containing COOKIE ECHO chunks on open ports, but send
-- an ABORT if the port is closed.
sctpCookieEchoScan :: LoudCxt s => NmapOptions s
sctpCookieEchoScan = NmapOptions "-sZ"

-- | IP protocol scan allows you to determine which IP protocols (TCP,
-- ICMP, IGMP, etc.) are supported by target machines. This isn't
-- technically a port scan, since it cycles through IP protocol
-- numbers rather than TCP or UDP port numbers.
ipProtocolScan :: LoudCxt s => NmapOptions s
ipProtocolScan  = NmapOptions "-sO"

-- | This advanced scan method allows for a truly blind TCP port scan
-- of the target (meaning no packets are sent to the target from your
-- real IP address). Instead, a unique side-channel attack exploits
-- predictable IP fragmentation ID sequence generation on the zombie
-- host to glean information about the open ports on the target.
--
-- Note set as quiet context due to use of side channel.
zombieHostScan :: QuietCxt s => Con Scannable -> NmapOptions s
zombieHostScan zombieHost  = NmapOptions ("-sI " ++ (getCon zombieHost))

-- | This vulnerability was widespread in 1997 when Nmap was released,
-- but has largely been fixed.
--
-- Note set as quiet context due to use of side channel.
ftpRelayScan :: QuietCxt s => Host Scannable -> NmapOptions s
ftpRelayScan relay  = NmapOptions ("-sI " ++ (getHost relay))

-- | Custom scan flags
scanFlagsScan :: LoudCxt s => String -> NmapOptions s
scanFlagsScan flags = NmapOptions ("--scanflags " ++ flags)
