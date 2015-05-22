-- |The MSF server can interact with a database where it stores things
-- like the discovered hosts, services, credentials, etc.

module MSF.DB
  ( module Types.DB
  , db_hosts
  , db_report_host
  , db_services
  , db_report_service
  , db_creds
  , db_report_cred
  , db_loots
  , db_report_loot
  , db_vulns
  , db_report_vuln
  , db_status
  , db_get_auth_info
  ) where

import MSF.Monad
import Types.DB
import qualified RPC.DB as RPC

-- |Get the list of hosts in the database.
db_hosts :: (SilentCxt s) => MSF s [HostInfo]
db_hosts = prim RPC.db_hosts

-- |Add a host to the database
db_report_host :: (SilentCxt s) => NewHost -> MSF s Result
db_report_host hst = prim $ \ addr auth ->
  RPC.db_report_host addr auth hst

-- |Get a list of the services in the database.
db_services :: (SilentCxt s) => MSF s [Service]
db_services = prim RPC.db_services

-- |Add a service to the database.
db_report_service :: (SilentCxt s) => NewService -> MSF s Result
db_report_service srv = prim $ \ addr auth ->
  RPC.db_report_service addr auth srv

-- |Get a list of the credentials in the database.
db_creds :: (SilentCxt s) => MSF s [Cred]
db_creds = prim RPC.db_creds

-- |Add a credential to the database.
db_report_cred :: (SilentCxt s) => NewCred -> MSF s Result
db_report_cred crd = prim $ \ addr auth ->
  RPC.db_report_cred addr auth crd

-- |Get a list of the loots in the database.
db_loots :: (SilentCxt s) => MSF s [Loot]
db_loots = prim RPC.db_loots

-- |Add a loot to the database.
db_report_loot :: (SilentCxt s) => NewLoot -> MSF s Result
db_report_loot loot = prim $ \ addr auth ->
  RPC.db_report_loot addr auth loot

-- |Get a list of the vulnerabilities in the database.
db_vulns :: (SilentCxt s) => MSF s Object
db_vulns = prim RPC.db_vulns

-- |Add a vulnerability to the database.
db_report_vuln :: (SilentCxt s) => NewVuln -> MSF s Result
db_report_vuln vln = prim $ \ addr auth ->
  RPC.db_report_vuln addr auth vln

-- XXX: add loudness constraint
db_status :: MSF s Object
db_status = prim RPC.db_status

-- XXX: add loudness constraint
db_get_auth_info :: MSF s Object
db_get_auth_info = prim RPC.db_get_auth_info

---- Other RPCs pulled from msf3/lib/msf/core/rpc/v10/rpc_db.rb
-- db.add_workspace(wspace)
-- db.clients(xopts)
-- db.connect(xopts)
-- db.current_workspace
-- db.del_client(xopts)
-- db.del_host(xopts)
-- db.del_note(xopts)
-- db.del_service(xopts)
-- db.del_vuln(xopts)
-- db.del_workspace(wspace)
-- db.disconnect
-- db.driver(xopts)
-- db.events(xopts)
-- db.get_client(xopts)
-- db.get_host(xopts)
-- db.get_note(xopts)
-- db.get_ref(name)
-- db.get_service(xopts)
-- db.get_vuln(xopts)
-- db.get_workspace(wspace)
-- db.import_data(xopts)
-- db.notes(xopts)
-- db.report_auth_info(xopts)
-- db.report_client(xopts)
-- db.report_cred(xopts)
-- db.report_event(xopts)
-- db.report_host(xopts)
-- db.report_loot(xopts)
-- db.report_note(xopts)
-- db.report_service(xopts)
-- db.report_vuln(xopts)
-- db.set_workspace(wspace)
-- db.workspaces

